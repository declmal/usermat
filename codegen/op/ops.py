import logging

import numpy as np
import mxnet as mx

from codegen.sign_utils import \
    infer_abs_sign, infer_add_sign, \
    infer_multiply_sign, infer_scalar_sign, \
    revinfer_multiply_sign, revinfer_add_sign, \
    OpSign, merge_sign
from codegen.op_utils import \
    One, MinusOne, Zero, FloatTypes, cast_fraction, \
    sequential_equiv_func, swappable_equiv_func
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op
from codegen.op.monomial import \
    get_monomial_dict, merge_monomial_dict, create_monomial_op
from codegen.op.polynomial import \
    get_polynomial_dict, merge_polynomial_dict, create_polynomial_op

""" validate functions
"""
def num_valid_func(num_deps):
    def wrapper(*deps):
        assert len(deps) == num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
    return wrapper


""" ops
"""
@org.register_op()
class Null(Op):
    pass


@org.register_opt("revtopo_infer_sign")
@org.register_op()
class Scalar(Op):
    def __init__(self, data):
        assert isinstance(data, FloatTypes)
        self.data = cast_fraction(data)
        super().__init__()

    def dfs_forward(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        val_dict[cop_id] = self.data

    def dfs_tosym(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        name = self.info(with_data=True)
        sym = mx.sym.var(name=name)
        val_dict[cop_id] = sym

    def dfs_display(
        self, val_dict, logger=logging.getLogger("op_info")):
        _info = self.info(val_dict, with_data=True)
        logger.debug(_info)

    def dfs_info(self, val_dict):
        _info = self.info(with_data=True)
        op_id = self.id
        val_dict[op_id] = _info

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        data = self.data
        sign = infer_scalar_sign(data)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            csign = merge_sign(sign, csign)
        val_dict[cop_id] = sign


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("dfs_info")
@org.register_opt("topo_fuse")
@org.register_opt("topo_standardize")
@org.register_opt("revtopo_infer_sign")
@org.register_op()
class Var(Op):
    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        return cls.default_op(*deps)

    def dfs_forward(self, val_dict):
        pass

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        cdiff[var_seq[cop_id]] = od.scalar(One)
        val_dict[cop_id] = cdiff


# standardized op
@org.register_opt("revtopo_infer_sign")
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_display")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func = lambda v: -v

    @classmethod
    def topo_standardize(cls, sign_dict, *deps):
        minus_one = od.scalar(-1)
        x = deps[0]
        op = od.multiply(x, minus_one)
        return op


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("topo_fuse")
@org.register_opt("topo_degenerate")
@org.register_opt("topo_standardize")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func = lambda v: np.sin(v)

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        x = self.deps[0]
        y = od.cos(x)
        xid = x.id
        xdiff = val_dict[xid]
        cdiff = []
        for di in xdiff:
            dop = od.multiply(y, di)
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = val_dict[dep_id]
        sign = OpSign.UNDEFINED
        if dep_sign == OpSign.ZERO:
            sign = OpSign.ZERO
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.NON_ZERO:
            x = self.deps[0]
            xid = x.id
            xsign = sign_dict[xid]
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            sign_dict[xid] = xsign
            return


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("dfs_info")
@org.register_opt("topo_standardize")
@org.register_opt("topo_fuse")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func = lambda v: np.abs(v)

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        if isinstance(dep, Scalar):
            op = od.scalar(dep.data)
            return op
        if dep_sign == OpSign.POSITIVE and dep_sign == OpSign.NON_NEGATIVE:
            return dep
        if dep_sign == OpSign.NEGATIVE and dep_sign == OpSign.NON_POSITIVE:
            op = od.multiply(dep, MinusOne)
            return op
        op = cls.default_op(*deps)
        return op

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        x = self.deps[0]
        xid = x.id
        xdiff = val_dict[xid]
        cdiff = []
        for di in xdiff:
            zero_op = od.scalar(0)
            neg_op = od.negative(di)
            dop = od.lessthan(di, zero_op, neg_op, di)
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = val_dict[dep_id]
        sign = infer_abs_sign(dep_sign)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        assert csign in \
            [OpSign.POSITIVE, OpSign.NON_NEGATIVE, OpSign.ZERO], csign
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        if csign == OpSign.POSITIVE:
            dep_sign = merge_sign(dep_sign, OpSign.NON_ZERO)
            sign_dict[dep_id] = dep_sign
            return
        if csign == OpSign.ZERO:
            dep_sign = merge_sign(dep_sign, OpSign.ZERO)
            sign_dict[dep_id] = dep_sign
            return


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("topo_fuse")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func = lambda v: np.cos(v)

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.ZERO:
            dep = self.deps[0]
            dep_id = dep.id
            dep_sign = sign_dict[dep_id]
            dep_sign = merge_sign(dep_sign, OpSign.NON_ZERO)
            sign_dict[dep_id] = dep_sign
            return


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("topo_standardize")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Add(Op):
    fwd_func = lambda v0, v1: v0 + v1

    def dfs_infer_sign(self, val_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = val_dict[xid], val_dict[yid]
        sign = infer_add_sign(xsign, ysign)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        x0, x1 = self.deps
        x0id, x1id = x0.id, x1.id
        d0, d1 = val_dict[x0id], val_dict[x1id]
        cdiff = []
        for i in range(len(var_seq)):
            dop = od.add(d0[i], d1[i])
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        x, y = deps
        x_dict = get_polynomial_dict(x)
        y_dict = get_polynomial_dict(y)
        m_dict = merge_polynomial_dict(x_dict, y_dict)
        op = create_polynomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero:
            return y
        if isinstance(y, Scalar) and y.data == Zero:
            return x
        return super().topo_degenerate(sign_dict, *deps)

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        sign = revinfer_add_sign(csign, xsign)
        ysign = merge_sign(sign, ysign)
        sign_dict[yid] = ysign
        sign = revinfer_add_sign(csign, ysign)
        xsign = merge_sign(sign, xsign)
        sign_dict[xid] = xsign


# standardized op
@org.register_opt("revtopo_infer_sign")
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_display")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func = lambda v0, v1: v0 - v1

    @classmethod
    def topo_standardize(cls, sign_dict, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        minus_y = od.multiply(minus_one, y)
        op = od.add(x, minus_y)
        return op


@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_display")
@org.register_opt("topo_standardize")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func = lambda v0, v1: v0 * v1

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        x0, x1 = self.deps
        x0id, x1id = x0.id, x1.id
        d0, d1 = val_dict[x0id], val_dict[x1id]
        cdiff = []
        for i in range(len(d0)):
            mul0 = od.multiply(x1, d0[i])
            mul1 = od.multiply(x0, d1[i])
            dop = od.add(mul0, mul1)
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = val_dict[xid], val_dict[yid]
        sign = infer_multiply_sign(xsign, ysign)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        x, y = deps
        x_dict = get_monomial_dict(x)
        y_dict = get_monomial_dict(y)
        m_dict = merge_monomial_dict(x_dict, y_dict)
        op = create_monomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero or \
            isinstance(y, Scalar) and y.data == Zero:
            return od.scalar(0)
        if isinstance(x, Scalar) and x.data == One:
            return y
        if isinstance(y, Scalar) and y.data == One:
            return x
        return super().topo_degenerate(sign_dict, *deps)

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        sign = revinfer_multiply_sign(csign, xsign)
        ysign = merge_sign(sign, ysign)
        sign_dict[yid] = ysign
        sign = revinfer_multiply_sign(csign, ysign)
        xsign = merge_sign(sign, xsign)
        sign_dict[xid] = xsign


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_display")
@org.register_opt("revtopo_infer_sign")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func = lambda v0, v1: v0 / v1

    @classmethod
    def topo_standardize(cls, sign_dict, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        _pow = od.power(y, minus_one)
        op = od.multiply(x, _pow)
        return op

import numpy as np
import mxnet as mx

from codegen.sign_utils import \
    infer_negative_sign, infer_nomorethan, infer_abs_sign, \
    infer_add_sign, infer_power_sign, infer_mutual_sign, \
    infer_lessthan, infer_multiply_sign, infer_scalar_sign, \
    OpSign, merge_signs, merge_sign
from codegen.op_utils import \
    One, MinusOne, Zero, FloatTypes, cast_fraction, \
    sequential_equiv_func, swappable_equiv_func
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op
from codegen.mials import \
    get_monomial_dict, merge_monomial_dict, get_polynomial_dict, \
    merge_polynomial_dict

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

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        sign = infer_scalar_sign(self.data)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
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


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
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

    def revtopo_infer_sign(self, sign_dict):
        op_id = self.id
        csign = sign_dict[op_id]
        if csign == OpSign.NON_ZERO:
            x = self.deps[0]
            xid = x.id
            xsign = sign_dict[xid]
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            sign_dict[xid] = xsign
            return


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
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
            osign = val_dict
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

    def revtopo_infer_sign(self, sign_dict):
        op_id = self.id
        csign = sign_dict[op_id]
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
@org.register_opt("topo_fuse")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func = lambda v: np.cos(v)

    def revtopo_infer_sign(self, sign_dict):
        op_id = self.id
        csign = sign_dict[op_id]
        if csign == OpSign.ZERO:
            dep = self.deps[0]
            dep_id = dep.id
            dep_sign = sign_dict[dep_id]
            dep_sign = merge_sign(dep_sign, OpSign.NON_ZERO)
            sign_dict[dep_id] = dep_sign
            return


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
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
            osign = val_dict
            sign = merge_sign(sign, osign)
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
        scalar = od.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = od.get_op(op_id)
            ndeps.append(dep)
            coef = od.scalar(scalar_data)
            ndeps.append(coef)
        if len(ndeps) == 3 and ndeps[0].data == Zero and \
            ndeps[2].data == One:
            return ndeps[1]
        op = od.polynomial(*ndeps)
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
        op_id = self.id
        csign = sign_dict[op_id]
        if csign == OpSign.UNDEFINED:
            return
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if xsign == OpSign.UNDEFINED or ysign == OpSign.UNDEFINED:
            return
        if csign == OpSign.NON_ZERO:
            if xsign == OpSign.ZERO:
                ysign = merge_sign(ysign, OpSign.NON_ZERO)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.ZERO:
                xsign = merge_sign(xsign, OpSign.NON_ZERO)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.ZERO:
            if xsign == OpSign.ZERO:
                ysign = merge_sign(ysign, OpSign.ZERO)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.NON_NEGATIVE:
                ysign = merge_sign(ysign, OpSign.NON_POSITIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.NON_POSITIVE:
                ysign = merge_sign(ysign, OpSign.NON_NEGATIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.POSITIVE:
                ysign = merge_sign(ysign, OpSign.NEGATIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.NEGATIVE:
                ysign = merge_sign(ysign, OpSign.POSITIVE)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.ZERO:
                xsign = merge_sign(xsign, OpSign.ZERO)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.NON_NEGATIVE:
                xsign = merge_sign(xsign, OpSign.NON_POSITIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.NON_POSITIVE:
                xsign = merge_sign(xsign, OpSign.NON_NEGATIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.POSITIVE:
                xsign = merge_sign(xsign, OpSign.NEGATIVE)
                sign_dict[xid] = xsign
                return
            if xsign == OpSign.NEGATIVE:
                xsign = merge_sign(xsign, OpSign.POSITIVE)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.NON_NEGATIVE:
            if xsign == OpSign.NEGATIVE:
                ysign = merge_sign(ysign, OpSign.POSITIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.NON_POSITIVE:
                ysign = merge_sign(ysign, OpSign.NON_NEGATIVE)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.NEGATIVE:
                xsign = merge_sign(xsign, OpSign.POSITIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.NON_POSITIVE:
                xsign = merge_sign(xsign, OpSign.NON_NEGATIVE)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.NON_POSITIVE:
            if xsign == OpSign.POSITIVE:
                ysign = merge_sign(ysign, OpSign.NEGATIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.NON_NEGATIVE:
                ysign = merge_sign(ysign, OpSign.NON_POSITIVE)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.POSITIVE:
                xsign = merge_sign(xsign, OpSign.NEGATIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.NON_NEGATIVE:
                xsign = merge_sign(xsign, OpSign.NON_POSITIVE)
                sign_dict[xid] = xsign
                return
            return
        lst2 = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
        if csign == OpSign.POSITIVE:
            if xsign in lst2:
                ysign = merge_sign(ysign, OpSign.POSITIVE)
                sign_dict[yid] = ysign
                return
            if ysign in lst2:
                xsign = merge_sign(xsign, OpSign.POSITIVE)
                sign_dict[xid] = xsign
                return
            return
        lst3 = [OpSign.NON_NEGATIVE, OpSign.POSITIVE]
        if csign == OpSign.NEGATIVE:
            if xsign in lst3:
                ysign = merge_sign(ysign, OpSign.NEGATIVE)
                sign_dict[yid] = ysign
                return
            if ysign in lst2:
                xsign = merge_sign(xsign, OpSign.NEGATIVE)
                sign_dict[xid] = xsign
                return
            return


# standardized op
@org.register_opt("revtopo_infer_sign")
@org.register_opt("dfs_infer_sign")
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
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        x, y = deps
        x_dict = get_monomial_dict(x)
        y_dict = get_monomial_dict(y)
        m_dict = merge_monomial_dict(x_dict, y_dict)
        scalar = od.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = od.get_op(op_id)
            ndeps.append(dep)
            exp = od.scalar(scalar_data)
            ndeps.append(exp)
        if len(ndeps) == 3 and ndeps[0].data == One and \
            ndeps[2].data == One:
            return ndeps[1]
        op = od.monomial(*ndeps)
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
        op_id = self.id
        csign = sign_dict[op_id]
        if csign == OpSign.UNDEFINED:
            return
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if csign == OpSign.NON_ZERO:
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            sign_dict[xid] = xsign
            ysign = merge_sign(ysign, OpSign.NON_ZERO)
            sign_dict[yid] = ysign
            return
        lst1 = [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]
        if csign == OpSign.ZERO:
            if xsign in lst1:
                ysign = merge_sign(ysign, OpSign.ZERO)
                sign_dict[yid] = ysign
                return
            if ysign in lst1:
                xsign = merge_sign(xsign, OpSign.ZERO)
                sign_dict[xid] = xsign
                return
            return
        if xsign == OpSign.UNDEFINED or ysign == OpSign.UNDEFINED:
            return
        lst2 = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
        lst3 = [OpSign.NON_NEGATIVE, OpSign.POSITIVE]
        if csign == OpSign.NON_NEGATIVE:
            if xsign in lst2:
                ysign = merge_sign(ysign, OpSign.NON_POSITIVE)
                sign_dict[yid] = ysign
                return
            if xsign in lst3:
                ysign = merge_sign(ysign, OpSign.NON_NEGATIVE)
                sign_dict[yid] = ysign
                return
            if ysign in lst2:
                xsign = merge_sign(xsign, OpSign.NON_POSITIVE)
                sign_dict[xid] = xsign
                return
            if ysign in lst3:
                xsign = merge_sign(xsign, OpSign.NON_NEGATIVE)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.NON_POSITIVE:
            if xsign in lst2:
                ysign = merge_sign(ysign, OpSign.NON_NEGATIVE)
                sign_dict[yid] = ysign
                return
            if xsign in lst3:
                ysign = merge_sign(ysign, OpSign.NON_POSITIVE)
                sign_dict[yid] = ysign
                return
            if ysign in lst2:
                xsign = merge_sign(xsign, OpSign.NON_NEGATIVE)
                sign_dict[xid] = xsign
                return
            if ysign in lst3:
                xsign = merge_sign(xsign, OpSign.NON_POSITIVE)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.POSITIVE:
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            ysign = merge_sign(ysign, OpSign.NON_ZERO)
            if xsign == OpSign.NEGATIVE:
                ysign = merge_sign(ysign, OpSign.NEGATIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.POSITIVE:
                ysign = merge_sign(ysign, OpSign.POSITIVE)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.NEGATIVE:
                xsign = merge_sign(xsign, OpSign.NEGATIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.POSITIVE:
                xsign = merge_sign(xsign, OpSign.POSITIVE)
                sign_dict[xid] = xsign
                return
            return
        if csign == OpSign.NEGATIVE:
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            ysign = merge_sign(ysign, OpSign.NON_ZERO)
            if xsign == OpSign.NEGATIVE:
                ysign = merge_sign(ysign, OpSign.POSITIVE)
                sign_dict[yid] = ysign
                return
            if xsign == OpSign.POSITIVE:
                ysign = merge_sign(ysign, OpSign.NEGATIVE)
                sign_dict[yid] = ysign
                return
            if ysign == OpSign.NEGATIVE:
                xsign = merge_sign(xsign, OpSign.POSITIVE)
                sign_dict[xid] = xsign
                return
            if ysign == OpSign.POSITIVE:
                xsign = merge_sign(xsign, OpSign.NEGATIVE)
                sign_dict[xid] = xsign
                return
            return


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
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

def cnd_auto_backward(deps, od_func, val_dict, var_seq):
    lhs, rhs, lv, rv = deps
    lv_id, rv_id = lv.id, rv.id
    dl, dr = val_dict[lv_id], val_dict[rv_id]
    cdiff = []
    for i in range(len(dl)):
        dop = od_func(lhs, rhs, dl[i], dr[i])
        cdiff.append(dop)
    return cdiff


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("topo_standardize")
@org.register_opt("topo_fuse")
@org.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, val_dict, var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        ids = [dep.id for dep in self.deps]
        asign, bsign, xsign, ysign = [val_dict[i] for i in ids]
        if infer_lessthan(asign, bsign):
            sign = xsign
        elif infer_nomorethan(bsign, asign):
            sign = ysign
        else:
            sign = infer_mutual_sign(xsign, ysign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(sign_dict, *deps)

    def revtopo_infer_sign(self, sign_dict):
        op_id = self.id
        sign = sign_dict[op_id]
        ids = [dep.id for dep in self.deps]
        aid, bid, xid, yid = ids
        asign, bsign, xsign, ysign = [sign_dict[i] for i in ids]
        if infer_lessthan(asign, bsign):
            xsign = merge_sign(xsign, sign)
            sign_dict[xid] = xsign
            return
        if infer_nomorethan(bsign, asign):
            ysign = merge_sign(ysign, sign)
            sign_dict[yid] = ysign
            return


@org.register_opt("topo_standardize")
@org.register_opt("topo_fuse")
@org.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class NoMoreThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(sign_dict, *deps)

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, val_dict, var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        ids = [dep.id for dep in self.deps]
        asign, bsign, xsign, ysign = [val_dict[i] for i in ids]
        if infer_nomorethan(asign, bsign):
            sign = xsign
        elif infer_lessthan(bsign, asign):
            sign = ysign
        else:
            sign = infer_mutual_sign(xsign, ysign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

    def revtopo_infer_sign(self, sign_dict):
        op_id = self.id
        sign = sign[op_id]
        ids = [dep.id for dep in self.deps]
        aid, bid, xid, yid = ids
        asign, bsign, xsign, ysign = [sign_dict[i] for i in ids]
        if infer_nomorethan(asign, bsign):
            xsign = merge_sign(xsign, sign)
            sign_dict[xid] = xsign
            return
        if infer_lessthan(bsign, asign):
            ysign = merge_sign(ysign, sign)
            sign_dict[yid] = ysign
            return

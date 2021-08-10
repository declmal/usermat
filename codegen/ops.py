import numpy as np
import mxnet as mx

from codegen.sign_utils import \
    infer_negative_sign, infer_nomorethan_sign, infer_abs_sign, \
    infer_add_sign, infer_power_sign, infer_mutual_sign, \
    infer_lessthan_sign, infer_multiply_sign, OpSign, merge_signs, \
    merge_sign
from codegen.op_utils import \
    One, MinusOne, Zero, FloatTypes, cast_fraction, \
    sequential_equiv_func, swappable_equiv_func
from codegen.op_def import OpDef as od
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
@od.register_op()
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
        if self.data == Zero:
            sign = OpSign.ZERO
        elif self.data > Zero:
            sign = OpSign.POSITIVE
        else:
            sign = OpSign.NEGATIVE
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign


@od.register_opt("dfs_infer_sign")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_fuse")
@od.register_opt("topo_standardize")
@od.register_op()
class Var(Op):
    @classmethod
    def topo_degenerate(cls, *deps):
        return cls.default_op(*deps)

    def dfs_forward(self, val_dict):
        pass

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        cdiff[var_seq[cop_id]] = od.scalar(One)
        val_dict[cop_id] = cdiff

    def revtopo_propagate_assertion(self):
        pass


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func = lambda v: -v

    @classmethod
    def topo_standardize(cls, deps):
        minus_one = od.scalar(-1)
        x = deps
        op = od.multiply(x, minus_one)
        return op


@od.register_opt("dfs_infer_sign")
@od.register_opt("dfs_forward")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_fuse")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_standardize")
@od.register_op(
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


@od.register_opt("dfs_forward")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func = lambda v: np.abs(v)

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


@od.register_opt("dfs_forward")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func = lambda v: np.cos(v)


@od.register_opt("dfs_forward")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Add(Op):
    fwd_func = lambda v0, v1: v0 + v1

    @classmethod
    def topo_fuse(cls, *deps):
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

    def dfs_infer_sign(self, val_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        x_sign, y_sign = val_dict[xid], val_dict[yid]
        sign = infer_add_sign(x_sign, y_sign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

    @classmethod
    def topo_degenerate(cls, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero:
            return y
        if isinstance(y, Scalar) and y.data == Zero:
            return x
        return super().topo_degenerate(*deps)

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

    def revtopo_propagate_(self):
        self.assertions = merge_assertions(self.assertions)
        sign = self.assertions[0]


@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func = lambda v0, v1: v0 - v1

    @classmethod
    def topo_standardize(cls, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        minus_y = od.multiply(minus_one, y)
        op = od.add(x, minus_y)
        return op


@od.register_opt("dfs_tosym")
@od.register_opt("dfs_forward")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func = lambda v0, v1: v0 * v1

    @classmethod
    def topo_fuse(cls, *deps):
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
    def topo_degenerate(cls, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero or \
            isinstance(y, Scalar) and y.data == Zero:
            return od.scalar(0)
        if isinstance(x, Scalar) and x.data == One:
            return y
        if isinstance(y, Scalar) and y.data == One:
            return x
        return super().topo_degenerate(*deps)

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
        x_sign, y_sign = val_dict[xid], val_dict[yid]
        sign = infer_multiply_sign(x_sign, y_sign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign


@od.register_opt("dfs_tosym")
@od.register_opt("dfs_forward")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func = lambda v0, v1: v0 / v1

    @classmethod
    def topo_standardize(cls, *deps):
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


@od.register_opt("dfs_forward")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, val_dict, var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        ids = [dep.id for dep in self.deps]
        a_sign, b_sign, x_sign, y_sign = [val_dict[i] for i in ids]
        if infer_lessthan_sign(a_sign, b_sign):
            sign = x_sign
        elif infer_nomorethan_sign(b_sign, a_sign):
            sign = y_sign
        else:
            sign = infer_mutual_sign(x_sign, y_sign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign


@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class NoMoreThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, val_dict, var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        ids = [dep.id for dep in self.deps]
        a_sign, b_sign, x_sign, y_sign = [val_dict[i] for i in ids]
        if infer_nomorethan_sign(a_sign, b_sign):
            sign = x_sign
        elif infer_lessthan_sign(b_sign, a_sign):
            sign = y_sign
        else:
            sign = infer_mutual_sign(x_sign, y_sign)
        cop_id = self.id
        if cop_id in val_dict:
            osign = val_dict[cop_id]
            sign = merge_sign(sign, osign)
        val_dict[cop_id] = sign

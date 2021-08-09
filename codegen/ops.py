import numpy as np
import mxnet as mx

from codegen.infer_utils import \
    infer_negative_sign, infer_nomorethan_sign, infer_abs_sign, \
    infer_add_sign, infer_power_sign, infer_mutual_sign, \
    infer_lessthan_sign, infer_multiply_sign, OpSign
from codegen.op_utils import \
    One, MinusOne, Zero, FloatTypes, cast_fraction, \
    sequential_equiv_func, swappable_equiv_func
from codegen.op_def import Op, OpDef as od
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
@od.register_opt("dfs_info")
@od.register_op()
class Scalar(Op):
    def __init__(self, data):
        assert isinstance(data, FloatTypes)
        self.data = cast_fraction(data)
        super().__init__()

    def infer_sign(self):
        if self.data == Zero:
            return OpSign.ZERO
        if self.data > Zero:
            return OpSign.POSITIVE
        return OpSign.NEGATIVE

    def dfs_forward(self, val_dict):
        op_id = self.id
        assert self.id not in val_dict
        val_dict[op_id] = self.data

    def dfs_tosym(self, sym_dict):
        op_id = self.id
        assert op_id not in sym_dict
        info_func = od.get_opt(self, "dfs_info")
        name = info_func(self, with_data=True)
        sym = mx.sym.var(name=name)
        sym_dict[op_id] = sym

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        diff_dict[cop_id] = cdiff


@od.register_opt("dfs_info")
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

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        cdiff[var_seq[cop_id]] = od.scalar(One)
        diff_dict[cop_id] = cdiff


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func = lambda v: -v

    def infer_sign(self):
        dep_sign = self.deps[0].get_sign()
        sign = infer_negative_sign(dep_sign)
        return sign

    @classmethod
    def topo_standardize(cls, deps):
        minus_one = od.scalar(-1)
        x = deps
        op = od.multiply(x, minus_one)
        return op


@od.register_opt("dfs_forward")
@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_fuse")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func = lambda v: np.sin(v)

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        x = self.deps[0]
        y = od.cos(x)
        xid = x.id
        xdiff = diff_dict[xid]
        cdiff = []
        for di in xdiff:
            dop = od.multiply(y, di)
            cdiff.append(dop)
        diff_dict[cop_id] = cdiff


@od.register_opt("dfs_forward")
@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func = lambda v: np.abs(v)

    def infer_sign(self):
        dep_sign = self.deps[0].get_sign()
        sign = infer_abs_sign(dep_sign)
        return sign

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        x = self.deps[0]
        xid = x.id
        xdiff = diff_dict[xid]
        cdiff = []
        for di in xdiff:
            zero_op = od.scalar(0)
            neg_op = od.negative(di)
            dop = od.lessthan(di, zero_op, neg_op, di)
            cdiff.append(dop)
        diff_dict[cop_id] = cdiff


@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func = lambda v: np.cos(v)


@od.register_opt("dfs_forward")
@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Add(Op):
    fwd_func = lambda v0, v1: v0 + v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_add_sign(x_sign, y_sign)
        return sign

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

    @classmethod
    def topo_degenerate(cls, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero:
            return y
        if isinstance(y, Scalar) and y.data == Zero:
            return x
        return super().topo_degenerate(*deps)

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        x0, x1 = self.deps
        x0id, x1id = x0.id, x1.id
        d0, d1 = diff_dict[x0id], diff_dict[x1id]
        cdiff = []
        for i in range(len(var_seq)):
            dop = od.add(d0[i], d1[i])
            cdiff.append(dop)
        diff_dict[cop_id] = cdiff


@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func = lambda v0, v1: v0 - v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_negative_sign(y_sign)
        sign = infer_add_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        minus_y = od.multiply(minus_one, y)
        op = od.add(x, minus_y)
        return op


@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("dfs_forward")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func = lambda v0, v1: v0 * v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_multiply_sign(x_sign, y_sign)
        return sign

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

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        x0, x1 = self.deps
        x0id, x1id = x0.id, x1.id
        d0, d1 = diff_dict[x0id], diff_dict[x1id]
        cdict = []
        for i in range(len(d0)):
            mul0 = od.multiply(x1, d0[i])
            mul1 = od.multiply(x0, d1[i])
            dop = od.add(mul0, mul1)
            cdiff.append(dop)
        diff_dict[cop_id] = cdiff


@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("dfs_forward")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func = lambda v0, v1: v0 / v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_power_sign(y_sign, MinusOne)
        sign = infer_multiply_sign(x_sign, ny_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        _pow = od.power(y, minus_one)
        op = od.multiply(x, _pow)
        return op

def cnd_auto_backward(deps, od_func, diff_dict, var_seq):
    lhs, rhs, lv, rv = deps
    lv_id, rv_id = lv.id, rv.id
    dl, dr = diff_dict[lv_id], diff_dict[rv_id]
    cdiff = []
    for i in range(len(dl)):
        dop = od_func(lhs, rhs, dl[i], dr[i])
        cdiff.append(dop)
    return cdiff


@od.register_opt("dfs_forward")
@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def infer_sign(self):
        a_sign, b_sign, x_sign, y_sign = \
            [dep.get_sign() for dep in self.deps]
        if infer_lessthan_sign(a_sign, b_sign):
            return x_sign
        if infer_nomorethan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, diff_dict, var_seq)
        diff_dict[cop_id] = cdiff


@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class NoMoreThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    def infer_sign(self):
        a_sign, b_sign, x_sign, y_sign = \
            [dep.get_sign() for dep in self.deps]
        if infer_nomorethan_sign(a_sign, b_sign):
            return x_sign
        if infer_lessthan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        od_func = getattr(od, self.op_type)
        cdiff = cnd_auto_backward(self.deps, od_func, diff_dict, var_seq)
        diff_dict[cop_id] = cdiff

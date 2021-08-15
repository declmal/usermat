from ..sign_utils import \
    infer_add_sign, infer_multiply_sign, \
    revinfer_multiply_sign, revinfer_add_sign, merge_sign
from ..type_utils import \
    Zero, One, sequential_equiv_func, swappable_equiv_func
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op, num_valid_func
from .monomial import \
    get_monomial_dict, merge_monomial_dict, create_monomial_op
from .polynomial import \
    get_polynomial_dict, merge_polynomial_dict, create_polynomial_op


""" ops
"""
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
        if isinstance(x, org.get_op_cls("scalar")) and x.data == Zero:
            return y
        if isinstance(y, org.get_op_cls("scalar")) and y.data == Zero:
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
        if isinstance(x, org.get_op_cls("scalar")) and x.data == Zero or \
            isinstance(y, org.get_op_cls("scalar")) and y.data == Zero:
            return od.scalar(0)
        if isinstance(x, org.get_op_cls("scalar")) and x.data == One:
            return y
        if isinstance(y, org.get_op_cls("scalar")) and y.data == One:
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


# standardized op
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

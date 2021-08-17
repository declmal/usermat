import numpy as np

from ..utils.sign_utils import \
    infer_negative_sign,infer_abs_sign, OpSign, merge_sign, insert_sign
from ..utils.type_utils import MinusOne
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import num_valid_func, sequential_equiv_func


""" ops
"""
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("dfs_info")
@org.register_opt("dfs_sort_deps")
@org.register_opt("topo_standardize")
@org.register_opt("topo_fuse")
@org.register_opt("topo_zerify")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func = lambda v: np.abs(v)

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        if isinstance(dep, org.get_op_cls("scalar")):
            dep_data = dep.data
            abs_data = abs(dep_data)
            op = od.scalar(abs_data)
            return op
        if dep_sign in [OpSign.POSITIVE, OpSign.NON_NEGATIVE]:
            return dep
        if dep_sign in [OpSign.NEGATIVE, OpSign.NON_POSITIVE]:
            scalar_minusone = od.scalar(MinusOne)
            op = od.multiply(dep, scalar_minusone)
            op_id = op.id
            sign = infer_negative_sign(dep_sign)
            insert_sign(op_id, sign_dict, sign)
            return op
        op = cls.default_op(*deps)
        return op

    def dfs_autodiff(self, val_dict, var_seq):
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
@org.register_opt("dfs_info")
@org.register_opt("dfs_sort_deps")
@org.register_opt("topo_fuse")
@org.register_opt("topo_degenerate")
@org.register_opt("topo_standardize")
@org.register_opt("topo_zerify")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func = lambda v: np.sin(v)

    def dfs_autodiff(self, val_dict, var_seq):
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


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_info")
@org.register_opt("dfs_display")
@org.register_opt("dfs_sort_deps")
@org.register_opt("topo_fuse")
@org.register_opt("topo_degenerate")
@org.register_opt("topo_zerify")
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

from ..type_utils import Zero
from ..sign_utils import \
    merge_sign, infer_nomorethan, infer_lessthan, OpSign
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import \
    num_valid_func, sequential_equiv_func, swappable_equiv_func, \
    swap_sort_deps


""" ops
"""
@org.register_opt("dfs_sort_deps")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class AssertLessThan(Op):
    def fwd_func(cls, v0, v1):
        assert v0 < v1
        return 1.0

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if infer_nomorethan(ysign, xsign):
            raise ContradictError
        if infer_lessthan(xsign, ysign):
            op = od.null()
            return op
        op = cls.default_op(*deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if xsign in [OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
            ysign = merge_sign(ysign, OpSign.POSITIVE)
            sign_dict[yid] = ysign
            return
        if ysign in [OpSign.ZERO, OpSign.NON_POSITIVE, OpSign.NEGATIVE]:
            xsign = merge_sign(xsign, OpSign.NEGATIVE)
            sign_dict[xid] = xsign
            return


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_infer_sign")
@org.register_opt("topo_zerify")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class AssertNoMoreThan(Op):
    def fwd_func(cls, v0, v1):
        assert v0 <= v1
        return 1.0

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if infer_lessthan(ysign, xsign):
            raise ContradictError
        if infer_nomorethan(xsign, ysign):
            op = od.null()
            return op
        op = cls.default_op(*deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if xsign == OpSign.POSITIVE:
            ysign = merge_sign(ysign, OpSign.POSITIVE)
            sign_dict[yid] = ysign
            return
        if xsign in [OpSign.ZERO, OpSign.NON_NEGATIVE]:
            ysign = merge_sign(ysign, OpSign.NON_NEGATIVE)
            sign_dict[yid] = ysign
            return
        if ysign in [OpSign.ZERO, OpSign.NON_POSITIVE]:
            xsign = merge_sign(xsign, OpSign.NON_POSITIVE)
            sign_dict[xid] = xsign
            return
        if ysign == OpSign.NEGATIVE:
            xsign = merge_sign(xsign, OpSign.NEGATIVE)
            sign_dict[xid] = xsign
            return


@org.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class AssertNotEqual(Op):
    def fwd_func(cls, v0, v1):
        assert v0 != v1
        return 1.0

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if infer_notequal(xsign, ysign):
            op = od.null()
            return op
        op = cls.default_op(*deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if xsign == OpSign.ZERO:
            ysign = merge_sign(ysign, OpSign.NON_ZERO)
            sign_dict[yid] = ysign
            return
        if ysign == OpSign.ZERO:
            xsign = merge_sign(xsign, OpSign.NON_ZERO)
            sign_dict[xid] = xsign
            return

    def dfs_sort_deps(self, val_dict):
        deps = self.deps
        ndeps = swap_sort_deps(*deps)
        valid_func = num_valid_func(2)
        valid_func(*ndeps)
        self.deps = ndeps


@org.register_opt("dfs_tosym")
@org.register_opt("dfs_infer_sign")
@org.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class AssertEqual(Op):
    def fwd_func(cls, v0, v1):
        assert v0 == v1
        return 1.0

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        x, y = deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        if xsign == ysign == OpSign.ZERO:
            op = od.null()
            return op
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        ndeps = []
        for dep in deps:
            if isinstance(dep, org.get_op_cls("scalar")):
                ndeps.append(dep)
                continue
            dep_id = dep.id
            sign = sign_dict[dep_id]
            if sign != OpSign.ZERO:
                ndeps.append(dep)
                continue
            od_sign = od.get_sign(dep_id)
            if od_sign != OpSign.ZERO:
                ndeps.append(dep)
                continue
            zero = od.scalar(Zero)
            ndeps.append(zero)
        return cls.default_op(*ndeps)

    def revtopo_infer_sign(self, sign_dict):
        x, y = self.deps
        xid, yid = x.id, y.id
        xsign, ysign = sign_dict[xid], sign_dict[yid]
        ysign = merge_sign(xsign, ysign)
        sign_dict[yid] = ysign
        xsign = merge_sign(xsign, ysign)
        sign_dict[xid] = xsign

    def dfs_sort_deps(self, val_dict):
        deps = self.deps
        ndeps = swap_sort_deps(*deps)
        valid_func = num_valid_func(2)
        valid_func(*ndeps)
        self.deps = ndeps

from ..sign_utils import \
    infer_nomorethan, infer_mutual_sign, infer_lessthan, \
    OpSign, merge_sign
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import num_valid_func, sequential_equiv_func

""" util functions
"""
def cnd_auto_backward(deps, od_func, val_dict, var_seq):
    lhs, rhs, lv, rv = deps
    lv_id, rv_id = lv.id, rv.id
    dl, dr = val_dict[lv_id], val_dict[rv_id]
    cdiff = []
    for i in range(len(dl)):
        dop = od_func(lhs, rhs, dl[i], dr[i])
        cdiff.append(dop)
    return cdiff


""" ops
"""
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_info")
@org.register_opt("topo_standardize")
@org.register_opt("topo_fuse")
@org.register_opt("topo_zerify")
@org.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def dfs_autodiff(self, val_dict, var_seq):
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
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(sign_dict, *deps)

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        sign = sign_dict[cop_id]
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

    def dfs_autodiff(self, val_dict, var_seq):
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
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        sign = sign[cop_id]
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

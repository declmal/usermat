from ..utils.type_utils import (
    Zero, Half, PositiveInf, NegativeInf, get_infsimal)
from .op_utils import sequential_equiv_func
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op

""" util function
"""
def update_min_max(minv, maxv, v):
    nminv = min(minv, v)
    nmaxv = max(maxv, v)
    return nminv, nmaxv

def get_critical_points(*deps):
    datas = [dep.data for dep in deps[1:]]
    ret = []
    # leftmost
    k, b, spt, c = datas[:4]
    if k == Zero:
        point = (NegativeInf, b)
    elif k > Zero:
        point = (NegativeInf, NegativeInf)
    else:
        point = (NegativeInf, PositiveInf)
    ret.append(point)
    raise NotImplementedError
    # rightmost
    k, b = datas[-2:]
    if k == Zero:
        point = (PositiveInf, b)
    elif k > Zero:
        point = (PositiveInf, PositiveInf)
    else:
        point = (NegativeInf, NegativeInf)
    ret.append(point)
    return ret

def get_negative_threshold(*deps):
    raise NotImplementedError

def get_positive_threshold(*deps):
    raise NotImplementedError

""" validate function
"""
def piecewise_linear_valid_func(*deps):
    num_deps = len(deps)
    assert num_deps >= 7 and (num_deps-3) % 4 == 0, \
        "invalid number of deps: {}".format(num_deps)
    for dep in deps:
        assert isinstance(dep, Op), \
            "invalid type of dep: {}".format(type(dep))
    split_points = []
    for i in range(1, num_deps):
        dep = deps[i]
        assert isinstance(dep, org.get_op_cls("scalar")), \
            "invalid type of dep: {}".format(type(dep))
        data = dep.data
        assert isinstance(data, FloatTypes), \
            "invalid type of data: {}".format(type(data))
        assert data not in [PositiveInf, NegativeInf], \
            "invalid value of data: {}".format(data)
        if (i-3) % 4 == 0:
            if split_points:
                pdata = split_points[-1]
                assert data > pdata, \
                    "invalid split points, data: {}, i: {}".format(data, i)
            split_points.append(data)


""" ops
"""
@org.register_op(
    valid_func=piecewise_linear_valid_func,
    equiv_func=sequential_equiv_func)
class PiecewiseLinear(Op):
    @classmethod
    def fwd_func(cls, *v):
        x = v[0]
        for i in range(1, len(v), 4):
            k, b, spt, c = v[i:i+4]
            if v[0] < spt:
                ret = k*x + b
                return ret
            if v[0] == spt:
                return c
        k, b = v[-2:]
        ret = k*x + b
        return ret

    def dfs_autodiff(self, val_dict, var_seq):
        cop = self.id
        assert cop_id not in val_dict
        var = self.deps[0]
        var_id = var.id
        diff = val_dict[var_id]
        ndeps = [var]
        scalar_zero = od.scalar(Zero)
        for i in range(1, len(self.deps), 4):
            kop, bop, sptop, cop, k1op, b1op = self.deps[i:i+6]
            k = kop.data
            b = bop.data
            c = cop.data
            spt = sptop.data
            k1 = k1op.data
            b1 = b1op.data
            ndeps.append(scalar_zero)
            ndeps.append(kop)
            ndeps.append(sptop)
            dst = abs(k*spt+b-c)
            dst1 = abs(k1*spt+b1-c)
            if dst < dst1:
                ncop = kop
            elif dst > dst1:
                ncop = k1op
            else:
                nc = Half * (k+k1)
                ncop = od.scalar(nc)
            ndeps.append(ncop)
        kop = self.deps[-2]
        ndeps.append(scalar_zero)
        ndeps.append(kop)
        nop = od.piecewiselinear(*ndeps)
        cdiff = []
        for d in diff:
            dop = od.multiply(nop, d)
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict, var_seq):
        var = self.deps[0]
        var_id = var.id
        var_sign = val_dict[var_id]

        # TODO
        raise NotImplementedError

    def rev_topo_infer_sign(self, sign_dict):
        # TODO
        raise NotImplementedError

from ..utils.math_utils import (
    validate_scalar_data, validate_pl_scalar_datas,
    get_piecewise_linear_diff_info)
from ..utils.type_utils import (
    Zero, Half, One, MinusOne, PositiveInf, NegativeInf,
    get_infsimal, FloatTypes, cast_float)
from ..utils.sign_utils import OpSign, is_sub_sign, merge_sign
from .op_utils import sequential_equiv_func
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..expr_def import ExprDef as ed

""" threshold helper function
"""
def get_leftend_point(k, b, spt):
    for v in [k, b, spt]:
        validate_scalar_data(v)
    x = get_infsimal(spt, forward=True)
    init_y = k*spt + b
    if k == Zero:
        y = b
    elif k > Zero:
        y = get_infsimal(init_y, forward=True)
    else:
        y = get_infsimal(init_y, forward=False)
    return x, y

def get_rightend_point(k, b, spt):
    for v in [k, b, spt]:
        validate_scalar_data(v)
    x = get_infsimal(spt, forward=False)
    init_y = k*spt + b
    if k == Zero:
        y = b
    elif k > Zero:
        y = get_infsimal(init_y, forward=False)
    else:
        y = get_infsimal(init_y, forward=True)
    return x, y


def get_critical_points(*datas):
    validate_pl_scalar_datas(*datas)
    ret = []
    # left inf
    k, b, spt, c = datas[:4]
    x = NegativeInf
    if k == Zero:
        y = b
    elif k > Zero:
        y = NegativeInf
    else:
        y = PositiveInf
    ret.append((x,y))
    # right end of the leftmost segment
    x, y = get_rightend_point(k, b, spt)
    ret.append((x,y))
    # left most split
    ret.append((spt,c))
    # middle
    for i in range(2, len(datas)-4, 4):
        spt, c, k, b, spt1, c1 = datas[i:i+6]
        # left end
        x, y = get_leftend_point(k, b, spt)
        ret.append((x,y))
        # right end
        x, y = get_rightend_point(k, b, spt1)
        ret.append((x,y))
        # right split
        ret.append((spt1,c1))
    # left end of the rightmost segment
    spt, c, k, b = datas[-4:]
    x, y = get_leftend_point(k, b, spt)
    ret.append((x,y))
    # right inf
    x = PositiveInf
    if k == Zero:
        y = b
    elif k > Zero:
        y = PositiveInf
    else:
        y = NegativeInf
    ret.append((x,y))
    return ret

def get_segment_zero_point(k, b, mode="exact"):
    for v in [k, b]:
        validate_scalar_data(v)
    assert mode in ["exact", "forward", "backward"]
    if mode == "exact":
        x = Zero
        y = b
    elif mode == "forward":
        x, y = get_leftend_point(k, b, Zero)
    else:
        x, y = get_rightend_point(k, b, Zero)
    return x, y

def get_zero_point(*datas, mode="exact"):
    assert mode in ["exact", "forward", "backward"]
    validate_pl_scalar_datas(*datas)
    # split hit
    for i in range(0, len(datas)-2, 4):
        k, b, spt, c, k1, b1 = datas[i:i+6]
        if spt != Zero:
            continue
        if mode == "exact":
            x = spt
            y = c
        elif mode == "forward":
            x, y = get_leftend_point(k1, b1, spt)
        else:
            x, y = get_rightend_point(k, b, spt)
        return x, y
    # leftmost segment hit
    k, b, spt, c = datas[:4]
    if spt > Zero:
        x, y = get_segment_zero_point(k, b, mode=mode)
        return x, y
    # middle segment hit
    for i in range(2, len(datas)-4, 4):
        spt, c, k, b, spt1, c1 = datas[i:i+6]
        if spt > Zero or spt1 < Zero:
            continue
        x, y = get_segment_zero_point(k, b, mode=mode)
        return x, y
    # rightmost segment hit
    spt, c, k, b = datas[:4]
    x, y = get_segment_zero_point(k, b, mode=mode)
    return x, y

""" threhold functions
"""
def get_negative_threshold(*datas):
    validate_pl_scalar_datas(*datas)
    npoints = []
    points = get_critical_points(*datas)
    x, y = get_zero_point(*datas, mode="backward")
    points.append((x,y))
    minv, maxv = PositiveInf, NegativeInf
    for x, y in points:
        if x >= Zero:
            continue
        minv = min(y, minv)
        maxv = max(y, maxv)
    return minv, maxv

def get_positive_threshold(*datas):
    validate_pl_scalar_datas(*datas)
    npoints = []
    points = get_critical_points(*datas)
    x, y = get_zero_point(*datas, mode="forward")
    points.append((x,y))
    minv, maxv = PositiveInf, NegativeInf
    for x, y in points:
        if x <= Zero:
            continue
        minv = min(y, minv)
        maxv = max(y, maxv)
    return minv, maxv

def get_zero_threshold(*datas):
    _, y = get_zero_point(*datas, mode="exact")
    return y, y

def get_nonnegative_threshold(*datas):
    minv0, maxv0 = get_positive_threshold(*datas)
    minv1, maxv1 = get_zero_threshold(*datas)
    minv = min(minv0, minv1)
    maxv = max(max0, max1)
    return minv, maxv

def get_nonpositive_threshold(*datas):
    minv0, maxv0 = get_negative_threshold(*datas)
    minv1, maxv1 = get_zero_threshold(*datas)
    minv = min(minv0, minv1)
    maxv = max(max0, max1)
    return minv, maxv

def get_nonzero_threshold(*datas):
    minv0, maxv0 = get_negative_threshold(*datas)
    minv1, maxv1 = get_positive_threshold(*datas)
    minv = min(minv0, minv1)
    maxv = max(max0, max1)
    return minv, maxv

def get_threshold(*datas):
    validate_pl_scalar_datas(*datas)
    npoints = []
    points = get_critical_points(*datas)
    minv, maxv = PositiveInf, NegativeInf
    for x, y in points:
        minv = min(y, minv)
        maxv = max(y, maxv)
    return minv, maxv

""" infer util function
"""
def infer_piecewise_linear_sign(var_sign, *datas):
    if var_sign == OpSign.UNDEFINED:
        minv, maxv = get_threshold(*datas)
    elif var_sign == OpSign.ZERO:
        minv, maxv = get_zero_threshold(*datas)
    elif var_sign == OpSign.NON_ZERO:
        minv, maxv = get_nonzero_threshold(*datas)
    elif var_sign == OpSign.NON_NEGATIVE:
        minv, maxv = get_nonnegative_threshold(*datas)
    elif var_sign == OpSign.NON_POSITIVE:
        minv, maxv = get_nonpositive_threshold(*datas)
    elif var_sign == OpSign.POSITIVE:
        minv, maxv = get_positive_threshold(*datas)
    else:
        assert var_sign == OpSign.NEGATIVE, var_sign
        minv, maxv = get_negative_threshold(*datas)
    if maxv < Zero:
        sign = OpSign.NEGATIVE
    elif minv > Zero:
        sign = OpSign.POSITIVE
    elif minv == maxv == Zero:
        sign = OpSign.ZERO
    elif minv == Zero:
        sign = OpSign.NON_NEGATIVE
    elif maxv == Zero:
        sign = OpSign.NON_POSITIVE
    else:
        sign = OpSign.UNDEFINED
    return sign

def unique_sign(signs, csign):
    j = -1
    cnt = 0
    for i, sign in enumerate(signs):
        if is_sub_sign(sign, csign):
            cnt += 1
            j = i
    if cnt == 1:
        return i
    return -1

""" validate function
"""
def piecewise_linear_valid_func(*deps):
    num_deps = len(deps)
    assert num_deps >= 7 and (num_deps-3) % 4 == 0, \
        "invalid number of deps: {}".format(num_deps)
    for dep in deps:
        assert isinstance(dep, org.get_op_cls("op")), \
            "invalid type of dep: {}".format(type(dep))
    datas = []
    for i in range(1, num_deps):
        dep = deps[i]
        assert isinstance(dep, org.get_op_cls("scalar")), \
            "invalid type of dep: {}".format(type(dep))
        data = dep.data
        datas.append(data)
    validate_pl_scalar_datas(*datas)

""" ast functions
"""
def arith_linear(k, b, dep_name):
    assert isinstance(k, FloatTypes), \
        "invalid type: {} of k: {}".format(type(k), k)
    assert isinstance(b, FloatTypes), \
        "invalid type: {} of b: {}".format(type(b), b)
    assert isinstance(dep_name, str), \
        "invalid type: {} of dep_name: {}".format(type(dep_name), dep_name)
    if k == Zero:
        lhs = []
    elif k == One:
        lhs = [dep_name]
    elif k == MinusOne:
        lhs = ["-", dep_name]
    else:
        k_str = str(cast_float(k))
        lhs = [k, " * ", dep_name]
    if b == Zero:
        if lhs:
            ret = lhs
        else:
            ret = ["0.0"]
    elif b < Zero:
        nb = -b
        b_str = str(cast_float(nb))
        if lhs:
            ret = lhs + [" - ", b_str]
        else:
            ret = ["-", b_str]
    else:
        b_str = str(cast_float(b))
        if lhs:
            ret = lhs + [" + ", b_str]
        else:
            ret = [b_str]
    return ret


""" ops
"""
@org.register_opt("topo_standardize")
@org.register_opt("topo_degenerate")
@org.register_opt("topo_fuse")
@org.register_opt("topo_zerify")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_info")
@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_forward")
@org.register_op(
    valid_func=piecewise_linear_valid_func,
    equiv_func=sequential_equiv_func)
class PiecewiseLinear(org.get_op_cls("op")):
    @classmethod
    def fwd_func(cls, *v):
        x = v[0]
        for i in range(1, len(v)-2, 4):
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
        cop_id = self.id
        assert cop_id not in val_dict
        var = self.deps[0]
        var_id = var.id
        diff = val_dict[var_id]
        datas = [dep.data for dep in self.deps[1:]]
        ndatas = get_piecewise_linear_diff_info(*datas)
        nscalars = [od.scalar(data) for data in ndatas]
        ndeps = [var] + nscalars
        nop = od.piecewiselinear(*ndeps)
        cdiff = []
        for d in diff:
            dop = od.multiply(nop, d)
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        var = self.deps[0]
        var_id = var.id
        var_sign = val_dict[var_id]
        datas = [dep.data for dep in self.deps[1:]]
        sign = infer_piecewise_linear_sign(var_sign, *datas)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def dfs_ast(self, val_dict, variables, codeblocks):
        var_name = self.name
        dep_name = self.deps[0].name
        first = True
        for i in range(1, len(self.deps)-2, 4):
            k, b, spt, c, k1, b1 = [dep.data for dep in self.deps[i:i+6]]
            # first branch
            cref = k*spt + b
            if cref == c:
                bop = " .LE. "
            else:
                bop = " .LT. "
            spt_str = str(cast_float(spt))
            logicals = [dep_name, bop, spt_str]
            if first:
                with_else = False
                first = False
            else:
                with_else = True
            ifstmt = ed.ifstmt(*logicals, with_else=with_else)
            codeblocks.append(ifstmt)
            start_col_assign = ifstmt.inc_start_col()
            arithmetics = arith_linear(k, b, dep_name)
            assignment = ed.assignment(
                var_name, *arithmetics, start_col=start_col_assign)
            codeblocks.append(assignment)
            # second branch
            cref = k1*spt + b1
            if bop == " .LE. " or cref == c:
                continue
            bop = " .EQ. "
            logicals = [dep_name, bop, spt_str]
            ifstmt = ed.ifstmt(*logicals, with_else=True)
            codeblocks.append(ifstmt)
            c_str = str(cast_float(c))
            start_col_assign = ifstmt.inc_start_col()
            assignment = ed.assignment(
                var_name, c_str, start_col=start_col_assign)
            codeblocks.append(assignment)
        elsestmt = ed.elsestmt()
        codeblocks.append(elsestmt)
        k, b = [dep.data for dep in self.deps[-2:]]
        start_col_assign = elsestmt.inc_start_col()
        arithmetics = arith_linear(k, b, dep_name)
        assignment = ed.assignment(
            var_name, *arithmetics, start_col=start_col_assign)
        codeblocks.append(assignment)
        endifstmt = ed.endifstmt()
        codeblocks.append(endifstmt)
        variables.append(var_name)

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        var = self.deps[0]
        var_id = var.id
        var_sign = sign_dict[var_id]
        datas = [dep.data for dep in self.deps[1:]]
        infer_sign_0 = infer_piecewise_linear_sign(var_sign, *datas)
        if is_sub_sign(infer_sign_0, csign):
            return
        sign = OpSign.UNDEFINED
        if var_sign == OpSign.NON_NEGATIVE:
            signs = [OpSign.ZERO, OpSign.POSITIVE]
            ind = unique_sign(signs, csign)
            if ind != -1:
                sign = signs[i]
        elif var_sign == OpSign.NON_POSITIVE:
            signs = [OpSign.ZERO, OpSign.NEGATIVE]
            ind = unique_sign(signs, csign)
            if ind != -1:
                sign = signs[i]
        elif var_sign == OpSign.NON_ZERO:
            signs = [OpSign.POSITIVE, OpSign.NEGATIVE]
            ind = unique_sign(signs, csign)
            if ind != -1:
                sign = signs[i]
        elif var_sign == OpSign.UNDEFINED:
            for signs in [
                [OpSign.ZERO, OpSign.NEGATIVE, OpSign.POSITIVE],
                [OpSign.NON_NEGATIVE, OpSign.NEGATIVE],
                [OpSign.NON_POSITIVE, OpSign.POSITIVE],
                [OpSign.ZERO, OpSign.NON_ZERO]]:
                ind = unique_sign(signs, csign)
                if ind != -1:
                    sign = signs[i]
                    break
        var_sign = merge_sign(var_sign, sign)
        sign_dict[var_id] = var_sign

from ..utils.sign_utils import (
    infer_multiply_sign, infer_add_sign, infer_scalar_sign,
    merge_sign, separate_signs, OpSign, revinfer_multiply_sign
)
from ..utils.type_utils import Zero, One, ContradictError, cast_float
from ..op_reg import OpReg as org
from .op_utils import (
    mial_equiv_func, mial_valid_func, mial_sort_deps,
    get_polynomial_dict, merge_polynomial_dict, create_polynomial_op
)
from ..expr_def import ExprDef as ed

""" revinfer function
"""
def revinfer_polynomial_sign(deps, signs, ysign, lst, sign_dict):
    for i, sign in enumerate(signs):
        if sign not in signs:
            break
    if i == 0:
        scalar = deps[0]
        scalar_data = scalar.data
        scalar_sign = infer_scalar_sign(scalar_data)
        merge_sign(scalar_sign, ysign)
        return
    ind = 2*i - 1
    var, coef = deps[ind:ind+2]
    coef_data = coef.data
    coef_sign = infer_scalar_sign(coef_data)
    dep_sign = revinfer_multiply_sign(ysign, coef_sign)
    var_id = var.id
    var_sign = sign_dict[var_id]
    sign = merge_sign(var_sign, dep_sign)
    sign_dict[var_id] = sign

""" util functions
"""
def set_polynomial_deps_sign(deps, isign, sign_dict):
    for i in range(1, len(self.deps), 2):
        var, coef = self.deps[i:i+2]
        coef_data = coef.data
        coef_sign = infer_scalar_sign(coef_data)
        sign = revinfer_multiply_sign(isign, coef_data)
        var_id = var.id
        assert var_id in sign_dict
        var_sign = sign_dict[var_id]
        sign = merge_sign(var_sign, sign)
        sign_dict[var_id] = sign

def get_polynomial_signs(deps, sign_dict):
    scalar = deps[0]
    scalar_data = scalar.data
    sign = infer_scalar_sign(scalar_data)
    signs = [sign]
    for i in range(1, len(deps), 2):
        var, coef = deps[i:i+2]
        var_id = var.id
        var_sign = sign_dict[var_id]
        coef_data = coef.data
        coef_sign = infer_scalar_sign(coef_data)
        sign = infer_multiply_sign(var_sign, coef_sign)
        signs.append(sign)
    return signs


""" polynomial op
"""
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_info")
@org.register_opt("dfs_display")
@org.register_opt("topo_zerify")
@org.register_op(
    valid_func=mial_valid_func, equiv_func=mial_equiv_func)
class Polynomial(org.get_op_cls("op")):
    @classmethod
    def fwd_func(cls, *v):
        summation = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                summation += v[i]
            else:
                summation += v[i]*v[i+1]
        return summation

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            var, coef = deps[i:i+2]
            var_id = var.id
            coef_data = coef.data
            m_dict_var = get_polynomial_dict(var)
            nm_dict_var = m_dict_var.copy()
            for op_id, data in nm_dict_var.items():
                scalar_data = nm_dict_var[op_id]
                nscalar_data = coef_data * scalar_data
                m_dict_var[op_id] = nscalar_data
            m_dict = merge_polynomial_dict(m_dict_var, m_dict)
        op = create_polynomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            var, coef = deps[i:i+2]
            var_id = var.id
            coef_data = coef.data
            m_dict[var_id] = coef_data
        op = create_polynomial_op(m_dict)
        return op

    def dfs_infer_sign(self, val_dict):
        signs = get_polynomial_signs(self.deps, val_dict)
        csign = signs[0]
        for sign in signs[1:]:
            csign = infer_add_sign(sign, csign)
        cop_id = self.id
        if cop_id in val_dict:
            sign = val_dict[cop_id]
            csign = merge_sign(csign, sign)
        val_dict[cop_id] = csign

    def dfs_sort_deps(self, val_dict):
        deps = self.deps
        ndeps = mial_sort_deps(*deps)
        mial_valid_func(*ndeps)
        self.deps = ndeps

    def dfs_ast(self, val_dict, variables, exprs):
        var_name = self.name
        lhs = ed.stringlist(var_name)
        strings = []
        for i in range(1, len(self.deps), 2):
            var, scalar = self.deps[i:i+2]
            var_name = var.name
            scalar_data = scalar.data
            if scalar_data < Zero:
                scalar_data = -scalar_data
                bop = " - "
            else:
                bop = " + "
            strings.append(bop)
            strings.append(var_name)
            if scalar_data == One:
                continue
            strings.append(" * ")
            scalar_data_str = str(cast_float(scalar_data))
            strings.append(scalar_data_str)
        scalar = self.deps[0]
        bop = strings[0]
        scalar_data = scalar.data
        if scalar_data == Zero:
            strings.pop(0)
            if bop == " - ":
                strings.insert(0, "-")
        else:
            scalar_data_str = str(cast_float(scalar_data))
            strings.insert(0, scalar_data_str)
        rhs = ed.stringlist(*strings)
        expr = ed.assignment(lhs, rhs)
        variables.append(var_name)
        exprs.append(expr)


    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.UNDEFINED:
            return
        signs = get_polynomial_signs(self.deps, sign_dict)
        if csign == OpSign.NON_ZERO:
            lst = [OpSign.ZERO]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_ZERO, lst, sign_dict)
            return
        if csign == OpSign.ZERO:
            lst = [OpSign.ZERO]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                return
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.ZERO, lst, sign_dict)
            lst = [OpSign.NON_NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                set_polynomial_deps_sign(self.deps, OpSign.ZERO, sign_dict)
                return
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_POSITIVE, lst, sign_dict)
            lst = [OpSign.NON_POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                set_polynomial_deps_sign(self.deps, OpSign.ZERO, sign_dict)
                return
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_NEGATIVE, lst, sign_dict)
            lst = [OpSign.POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NEGATIVE, lst, sign_dict)
            lst = [OpSign.NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.POSITIVE, lst, sign_dict)
            return
        if csign == OpSign.NON_NEGATIVE:
            lst = [OpSign.NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.POSITIVE, lst, sign_dict)
            lst = [OpSign.NON_POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                set_polynomial_deps_sign(self.deps, OpSign.ZERO, sign_dict)
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_NEGATIVE, lst, sign_dict)
            return
        if csign == OpSign.NON_POSITIVE:
            lst = [OpSign.POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NEGATIVE, lst, sign_dict)
            lst = [OpSign.NON_NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                set_polynomial_deps_sign(self.deps, OpSign.ZERO, sign_dict)
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_POSITIVE, lst, sign_dict)
            return
        if csign == OpSign.POSITIVE:
            lst = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.POSITIVE, lst, sign_dict)
            return
        if csign == OpSign.NEGATIVE:
            lst = [OpSign.NON_NEGATIVE, OpSign.POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NEGATIVE, lst, sign_dict)
            return
        assert False

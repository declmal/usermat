from fractions import Fraction

from ..sign_utils import \
    infer_power_sign, infer_scalar_sign, infer_multiply_sign_consec, \
    revinfer_multiply_sign, revinfer_power_sign, \
    merge_sign, separate_signs, OpSign
from ..type_utils import One, Zero, ContradictError
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import \
    sequential_equiv_func, mial_valid_func, merge_monomial_dict, \
    create_monomial_op, get_monomial_dict_exp

""" validate function
"""
def monomial_valid_func(*deps):
    mial_valid_func(*deps)
    num_deps = len(deps)
    scalar = deps[0]
    scalar_data = scalar.data
    assert scalar_data != Zero
    for i in range(2, num_deps, 2):
        dep = deps[i]
        data = dep.data
        assert isinstance(data, Fraction), \
            "invalid type of data: {}".format(type(data))

""" revinfer function
"""
def revinfer_monomial_sign(deps, signs, ysign, lst, sign_dict):
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
    frac, exp = deps[ind:ind+2]
    exp_data = exp.data
    dep_sign = revinfer_power_sign(ysign, exp_data)
    frac_id = frac.id
    frac_sign = sign_dict[frac_id]
    sign = merge_sign(frac_sign, dep_sign)
    sign_dict[frac_id] = sign

""" util functions
"""
def set_monomial_deps_sign(deps, isign, sign_dict):
    for i in range(1, len(self.deps), 2):
        frac, exp = self.deps[i:i+2]
        exp_data = exp.data
        sign = revinfer_power_sign(isign, exp_data)
        frac_id = frac.id
        assert frac_id in sign_dict
        frac_sign = sign_dict[frac_id]
        sign = merge_sign(frac_sign, sign)
        sign_dict[frac_id] = sign

def get_monomial_signs(deps, sign_dict):
    scalar = deps[0]
    scalar_data = scalar.data
    sign = infer_scalar_sign(scalar_data)
    signs = [sign]
    for i in range(1, len(deps), 2):
        frac, exp = deps[i:i+2]
        frac_id = frac.id
        frac_sign = sign_dict[frac_id]
        exp_data = exp.data
        sign = infer_power_sign(frac_sign, exp_data)
        signs.append(sign)
    return signs


""" ops
"""
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_opt("dfs_info")
@org.register_opt("dfs_display")
@org.register_op(
    valid_func=monomial_valid_func, equiv_func=sequential_equiv_func)
class Monomial(Op):
    @classmethod
    def fwd_func(cls, *v):
        product = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                product *= v[i]
            else:
                product *= v[i]**v[i+1]
        return product

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            frac, exp = deps[i:i+2]
            frac_id = frac.id
            exp_data = exp.data
            m_dict_frac = get_monomial_dict_exp(frac, exp_data, sign_dict)
            m_dict = merge_monomial_dict(m_dict_frac, m_dict)
        op = create_monomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            frac, exp = deps[i:i+2]
            frac_id = frac.id
            frac_sign = sign_dict[frac_id]
            exp_data = exp.data
            deno, nume = exp_data.denominator, exp_data.numerator
            if deno == 1 and nume % 2 == 0 and \
                isinstance(frac, org.get_op_cls("abs")):
                dep = frac.deps[0]
                op_id = dep.id
            else:
                op_id = frac.id
            if op_id in m_dict:
                # test_power_3.py
                m_dict[op_id] += exp_data
            else:
                m_dict[op_id] = exp_data
        op = create_monomial_op(m_dict)
        return op

    def dfs_infer_sign(self, val_dict):
        signs = get_monomial_signs(self.deps, val_dict)
        csign = infer_multiply_sign_consec(signs)
        cop_id = self.id
        if cop_id in val_dict:
            sign = val_dict[cop_id]
            csign = merge_sign(csign, sign)
        val_dict[cop_id] = csign

    def revtopo_infer_sign(self, sign_dict):
        for i in range(1, len(self.deps), 2):
            frac, exp = self.deps[i:i+2]
            frac_id = frac.id
            frac_sign = sign_dict[frac_id]
            exp_data = exp.data
            deno, nume = exp_data.denominator, exp_data.numerator
            if deno == 1 and nume > 0:
                continue
            if deno == 1 and nume < 0:
                sign = merge_sign(frac_sign, OpSign.NON_ZERO)
                sign_dict[frac_id] = sign
                continue
            if deno > 1 and nume < 0:
                sign = merge_sign(frac_sign, OpSign.POSITIVE)
                sign_dict[frac_id] = sign
                continue
            if deno > 1 and nume > 0:
                sign = merge_sign(frac_sign, OpSign.NON_NEGATIVE)
                sign_dict[frac_id] = sign
                continue
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.UNDEFINED:
            return
        if csign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
            set_monomial_deps_sign(self.deps, OpSign.NON_ZERO, sign_dict)
        if csign == OpSign.NON_ZERO:
            return
        signs = get_monomial_signs(self.deps, sign_dict)
        if csign == OpSign.ZERO:
            lst = [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                raise ContradictError
            if len(signs2) == 1:
                revinfer_monomial_sign(
                    self.deps, signs, OpSign.ZERO, lst, sign_dict)
                return
            return
        lst = [OpSign.ZERO]
        signs1, _ = separate_signs(signs, lst)
        assert len(signs1) == 0
        lst = [
            OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO,
            OpSign.NON_POSITIVE, OpSign.NON_NEGATIVE]
        signs1, signs2 = separate_signs(signs, lst)
        for sign in signs2:
            assert sign == OpSign.UNDEFINED
        if len(signs2) > 1:
            return
        if len(signs2) == 1:
            xsign = infer_multiply_sign_consec(signs1)
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return
        lst = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
        signs1, signs2 = separate_signs(signs, lst)
        for sign in signs2:
            assert sign in [OpSign.NON_NEGATIVE, OpSign.NON_POSITIVE]
        xsign = infer_multiply_sign_consec(signs1)
        if len(signs2) == 0:
            merge_sign(xsign, csign)
            return
        if len(signs2) == 1:
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return
        lst = [
            OpSign.POSITIVE, OpSign.NEGATIVE,
            OpSign.NON_ZERO, OpSign.NON_NEGATIVE]
        signs1, signs2 = separate_signs(signs, lst)
        for sign in signs2:
            assert sign == OpSign.NON_POSITIVE
        xsign = infer_multiply_sign_consec(signs1)
        if len(signs2) == 0:
            merge_sign(xsign, csign)
            return
        if len(signs2) == 1:
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return
        lst = [
            OpSign.POSITIVE, OpSign.NEGATIVE,
            OpSign.NON_ZERO, OpSign.NON_POSITIVE]
        signs1, signs2 = separate_signs(signs, lst)
        for sign in signs2:
            assert sign == OpSign.NON_NEGATIVE
        xsign = infer_multiply_sign_consec(signs1)
        if len(signs2) == 0:
            merge_sign(xsign, csign)
            return
        if len(signs2) == 1:
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return

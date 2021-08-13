from fractions import Fraction
from codegen.sign_utils import \
    infer_power_sign, infer_multiply_sign, infer_add_sign, \
    infer_scalar_sign, infer_multiply_sign_consec, \
    merge_sign, separate_signs
from codegen.op_utils import \
    One, Zero, FloatTypes, validate_exp, sequential_equiv_func, \
    ContradictError
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op

""" mial validate function
"""
def mial_valid_func(mial_type):
    assert mial_type in ["poly", "mono"]

    def wrapper(*deps):
        num_deps = len(deps)
        assert num_deps >= 1 and num_deps % 2 == 1, \
            "invalid number of deps: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
        for i in range(0, num_deps, 2):
            dep = deps[i]
            assert isinstance(dep, org.get_op_cls("scalar")), \
                "invalid type of dep: {}".format(type(dep))
            data = dep.data
            assert isinstance(data, FloatTypes), \
                "invalid type of data: {}".format(type(data))
            if i > 0:
                assert data != Zero, \
                    "data could not be zero: {}".format(data)
        if mial_type == "poly" or num_deps == 1:
            return
        scalar = deps[0]
        scalar_data = scalar.data
        assert scalar_data != Zero
        for i in range(2, num_deps, 2):
            dep = deps[i]
            data = dep.data
            assert isinstance(data, Fraction), \
                "invalid type of data: {}".format(type(data))
    return wrapper

""" monomial util functions
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

def get_monomial_signs(deps):
    scalar = deps[0]
    scalar_data = scalar.data
    sign = infer_scalar_sign(scalar_data)
    signs = [sign]
    for i in range(1, len(deps), 2):
        frac, exp = deps[i:i+2]
        frac_id = frac.id
        frac_sign = val_dict[frac_id]
        exp_data = exp.data
        sign = infer_power_sign(frac_sign, exp_data)
        signs.append(sign)
    return signs

def validate_monomial_dict(m_dict):
    assert isinstance(m_dict, dict) and -1 in m_dict, m_dict
    for op_id, exp_data in m_dict.items():
        assert isinstance(op_id, int), type(op_id)
        assert isinstance(exp_data, FloatTypes), type(exp_data)
        if op_id == -1:
            continue
        assert isinstance(exp_data, Fraction), type(exp_data)
        frac = od.get_op(op_id)

def get_monomial_dict(op):
    if isinstance(op, org.get_op_cls("monomial")):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            frac, exp = op.deps[i:i+2]
            exp_data = exp.data
            frac_id = frac.id
            m_dict[frac_id] = exp_data
    elif isinstance(op, org.get_op_cls("scalar")):
        scalar_data = op.data
        m_dict = {-1: scalar_data}
    else:
        op_id = op.id
        m_dict = {-1: One, op_id: One}
    validate_monomial_dict(m_dict)
    return m_dict

def merge_monomial_dict(m_dict1, m_dict2):
    validate_monomial_dict(m_dict1)
    validate_monomial_dict(m_dict2)
    m_dict = m_dict2.copy()
    for op_id, scalar_data in m_dict1.items():
        if op_id == -1:
            m_dict[-1] *= scalar_data
            if m_dict[-1] == Zero:
                m_dict = {-1: Zero}
                break
            continue
        if op_id not in m_dict:
            m_dict[op_id] = scalar_data
            continue
        scalar_data2 = m_dict[op_id]
        _sum = scalar_data2 + scalar_data
        if _sum == Zero:
            del m_dict[op_id]
        else:
            m_dict[op_id] = _sum
    validate_monomial_dict(m_dict)
    return m_dict

def create_monomial_op(m_dict):
    validate_monomial_dict(m_dict)
    scalar_data = m_dict[-1]
    if len(m_dict) == 1:
        op = od.scalar(scalar_data)
        return op
    deps = []
    for op_id, exp_data in m_dict.items():
        if op_id == -1:
            continue
        nume = exp_data.numerator
        if nume == 0:
            continue
        frac = od.get_op(op_id)
        if isinstance(frac, org.get_op_cls("scalar")):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            inc = frac_data ** exp_data
            scalar_data *= inc
            continue
        deps.append(frac)
        exp = od.scalar(exp_data)
        deps.append(exp)
    if scalar_data == Zero:
        op = od.scalar(Zero)
        return op
    if len(deps) == 2 and scalar_data == One:
        exp = deps[1]
        exp_data = exp.data
        deno, nume = exp_data.denominator, exp_data.numerator
        if deno == 1 and nume == 1:
            op = deps[0]
            return op
    scalar = od.scalar(scalar_data)
    deps = [scalar] + deps
    op = od.monomial(*deps)
    return op

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

""" polynomial util functions
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

def get_polynomial_signs(deps):
    scalar = deps[0]
    scalar_data = scalar.data
    sign = infer_scalar_sign(scalar_data)
    signs = [sign]
    for i in range(1, len(deps), 2):
        var, coef = deps[i:i+2]
        var_id = var.id
        var_sign = val_dict[var_id]
        coef_id = coef.id
        coef_sign = val_dict[coef_id]
        sign = infer_multiply_sign(var_sign, coef_sign)
        signs.append(sign)
    return signs

def validate_polynomial_dict(m_dict):
    assert isinstance(m_dict, dict) and -1 in m_dict, m_dict
    for op_id, coef_data in m_dict.items():
        assert isinstance(op_id, int), type(op_id)
        assert isinstance(coef_data, FloatTypes), type(coef_data)
        if op_id == -1:
            continue
        frac = od.get_op(op_id)

def get_polynomial_dict(op):
    if isinstance(op, org.get_op_cls("polynomial")):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            var, coef = op.deps[i:i+2]
            coef_data = coef.data
            var_id = var.id
            m_dict[var_id] = coef_data
    elif isinstance(op, org.get_op_cls("scalar")):
        scalar_data = op.data
        m_dict = {-1: scalar_data}
    else:
        op_id = op.id
        m_dict = {-1: Zero, op_id: One}
    validate_polynomial_dict(m_dict)
    return m_dict

def merge_polynomial_dict(m_dict1, m_dict2):
    validate_polynomial_dict(m_dict1)
    validate_polynomial_dict(m_dict2)
    m_dict = m_dict2.copy()
    for op_id, scalar_data in m_dict1.items():
        if op_id == -1:
            m_dict[-1] += scalar_data
            continue
        if op_id not in m_dict:
            m_dict[op_id] = scalar_data
            continue
        scalar_data2 = m_dict[op_id]
        _sum = scalar_data2 + scalar_data
        if _sum == Zero:
            del m_dict[op_id]
        else:
            m_dict[op_id] = _sum
    validate_polynomial_dict(m_dict)
    return m_dict

def create_polynomial_op(m_dict):
    validate_polynomial_dict(m_dict)
    scalar_data = m_dict[-1]
    if len(m_dict) == 1:
        op = od.scalar(scalar_data)
        return op
    deps = []
    for op_id, coef_data in m_dict.items():
        if op_id == -1:
            continue
        if coef_data == Zero:
            continue
        var = od.get_op(op_id)
        if isinstance(var, org.get_op_cls("scalar")):
            var_data = var.data
            inc = var_data * coef_data
            scalar_data += inc
            continue
        deps.append(var)
        coef = od.scalar(coef_data)
        deps.append(coef)
    if len(deps) == 2 and scalar_data == Zero:
        coef = deps[1]
        coef_data = coef.data
        if coef_data == One:
            op = deps[0]
            return op
    scalar = od.scalar(scalar_data)
    deps = [scalar] + deps
    op = od.polynomial(*deps)
    return op

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


""" mial ops
"""
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_op(
    valid_func=mial_valid_func("mono"), equiv_func=sequential_equiv_func)
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
    def topo_degenerate(cls, sign_dict, *deps):
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            frac, exp = deps[i:i+2]
            frac_id = frac.id
            exp_data = exp.data
            m_dict[frac_id] = exp_data
        op = create_monomial_op(m_dict)
        return op

    def dfs_infer_sign(self, val_dict):
        signs = get_monomial_signs(self.deps)
        csign = infer_multiply_sign_consec(signs)
        cop_id = self.id
        if cop_id in val_dict:
            sign = val_dict[cop_id]
            csign = merge_sign(csign, sign)
        val_dict[cop_id] = csign

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.UNDEFINED:
            return
        if csign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
            set_monomial_deps_sign(self.deps, OpSign.NON_ZERO, sign_dict)
        if csign == OpSign.NON_ZERO:
            return
        signs = get_monomial_signs(deps)
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
            xsign = infer_multiply_sign_consec(signs)
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return
        lst = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
        signs1, signs2 = separate_signs(signs, lst)
        for sign in signs2:
            assert sign in [OpSign.NON_NEGATIVE, OpSign.NON_POSITIVE]
        xsign = infer_multiply_sign_consec(signs)
        if len(signs2) == 0:
            merge_sign(xsign, csign)
            return
        if len(signs2) == 1:
            ysign = revinfer_multiply_sign(csign, xsign)
            revinfer_monomial_sign(
                self.deps, signs, ysign, lst, sign_dict)
            return
        # TODO


@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_op(
    valid_func=mial_valid_func("poly"), equiv_func=sequential_equiv_func)
class Polynomial(Op):
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
        signs = get_polynomial_signs(self.deps)
        csign = signs[0]
        for sign in signs[1:]:
            csign = infer_add_sign(sign, csign)
        cop_id = self.id
        if cop_id in val_dict:
            sign = val_dict[cop_id]
            csign = merge_sign(csign, sign)
        val_dict[cop_id] = csign

    def revtopo_infer_sign(self, sign_dict):
        cop_id = self.id
        csign = sign_dict[cop_id]
        if csign == OpSign.UNDEFINED:
            return
        signs = get_polynomial_signs(self.deps)
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
                set_deps_sign(self.deps, OpSign.ZERO, sign_dict)
                return
            if len(signs2) == 1:
                revinfer_polynomial_sign(
                    self.deps, signs, OpSign.NON_POSITIVE, lst, sign_dict)
            lst = [OpSign.NON_POSITIVE]
            signs1, signs2 = separate_signs(signs, lst)
            if len(signs2) == 0:
                set_deps_sign(self.deps, OpSign.ZERO, sign_dict)
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
                set_deps_sign(self.deps, OpSign.ZERO, sign_dict)
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
                set_deps_sign(self.deps, OpSign.ZERO, sign_dict)
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

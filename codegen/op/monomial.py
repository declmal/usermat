from fractions import Fraction
from codegen.sign_utils import \
    infer_power_sign, infer_scalar_sign, infer_multiply_sign_consec, \
    infer_negative_sign, infer_abs_sign, \
    revinfer_multiply_sign, revinfer_power_sign, \
    merge_sign, separate_signs, OpSign, insert_sign
from codegen.type_utils import \
    One, Zero, MinusOne, FloatTypes, validate_exp, \
    sequential_equiv_func, ContradictError
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op

""" mial validate function
"""
def mial_valid_func(*deps):
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
    dep_ids = set()
    for i in range(1, num_deps, 2):
        dep = deps[i]
        dep_id = dep.id
        dep_ids.add(dep_id)
    num_fracs = (num_deps-1) // 2
    assert len(dep_ids) == num_fracs, \
        "duplicate var id, dep ids: {}, num_fracs: {}".format(
            dep_ids, num_fracs)

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

""" m_dict exp util function, only intended for fuse pass
"""
def get_monomial_dict_exp(frac, exp_data, sign_dict):
    deno, nume = exp_data.denominator, exp_data.numerator
    if deno == 1 and nume == 1:
        frac_id = frac.id
        m_dict = {-1: One, frac_id: One}
        return m_dict
    if nume == 0:
        m_dict = {-1: One}
        return m_dict
    assert not isinstance(frac, (org.get_op_cls("multiply"),
        org.get_op_cls("add"), org.get_op_cls("power")))
    m_dict = get_monomial_dict(frac)
    if len(m_dict) == 1 or m_dict[-1] == Zero:
        data = m_dict[-1]
        validate_exp(data, exp_data)
        scalar_data = data ** exp_data
        scalar = od.scalar(scalar_data)
        return scalar
    # separate m_dict
    if deno > 1:
        nm_dict = {}
        sm_dict = {}
        for op_id, data in m_dict.items():
            if op_id == -1:
                if data > Zero:
                    nm_dict[-1] = data
                    sm_dict[-1] = One
                else:
                    nm_dict[-1] = -data
                    sm_dict[-1] = MinusOne
                continue
            deno_in, nume_in = data.denominator, data.numerator
            sign = sign_dict[op_id]
            if nume_in % 2 == 0 and sign in \
                [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO] or \
                sign == OpSign.POSITIVE:
                nm_dict[op_id] = data
                continue
            sm_dict[op_id] = data
        flag = True
        for op_id, data in sm_dict.items():
            if op_id == -1:
                scalar_data = sm_dict[-1]
                if scalar_data == MinusOne:
                    flag = False
                    break
                continue
            deno_in, nume_in = data.denominator, data.numerator
            sign = sign_dict[op_id]
            if nume_in % 2 != 0 and sign not in \
                [OpSign.POSITIVE, OpSign.ZERO, OpSign.NON_NEGATIVE]:
                flag = False
                break
        if flag:
            for op_id, data in sm_dict.items():
                if op_id == -1:
                    continue
                assert op_id not in nm_dict
                nm_dict[op_id] = data
        else:
            op = create_monomial_op(sm_dict)
            op_id = op.id
            if op_id in nm_dict:
                # TODO: unittest
                nm_dict[op_id] += One
            else:
                nm_dict[op_id] = One
        m_dict = nm_dict
    nm_dict = m_dict.copy()
    # x^(even/odd)^[odd2/(k*even)] = |x|^[odd2/(k*odd)]
    for op_id, data in nm_dict.items():
        if op_id == -1:
            continue
        deno_in, nume_in = data.denominator, data.numerator
        if deno_in == 1 and nume_in % 2 == 0 and \
            deno >= nume_in and deno % nume_in == 0:
            sign = sign_dict[op_id]
            if sign in \
                [OpSign.POSITIVE, OpSign.NON_NEGATIVE, OpSign.ZERO]:
                continue
            cop = od.get_op(op_id)
            if sign in [OpSign.NEGATIVE, OpSign.NON_NEGATIVE]:
                m_dict_tmp = {-1: MinusOne, op_id: One}
                nop = create_monomial_op(m_dict_tmp)
                csign = infer_negative_sign(sign)
                nop_id = nop.id
                insert_sign(nop_id, sign_dict, csign)
            else:
                nop = od.abs(cop)
                csign = infer_abs_sign(sign)
                nop_id = nop.id
                insert_sign(nop_id, sign_dict, csign)
            del m_dict[op_id]
            nop_id = nop.id
            if nop_id not in m_dict:
                m_dict[nop_id] = data
            else:
                # unittest test_power_2.py
                m_dict[nop_id] += data
    # distribute exp_data
    nm_dict = m_dict.copy()
    for op_id, data in nm_dict.items():
        if op_id == -1:
            scalar_data = data ** exp_data
            m_dict[-1] = scalar_data
            continue
        ndata = data * exp_data
        m_dict[op_id] = ndata
    # fuse |x|^even = x^even
    nm_dict = m_dict.copy()
    for op_id, data in nm_dict.items():
        if op_id == -1:
            continue
        deno_in, nume_in = data.denominator, data.numerator
        op = od.get_op(op_id)
        if deno_in == 1 and nume_in % 2 == 0 and \
            isinstance(op, org.get_op_cls("abs")):
            del m_dict[op_id]
            sop = op.deps[0]
            sid = sop.id
            if sid not in m_dict:
                m_dict[sid] = data
            else:
                # unittest test_power_1.py
                m_dict[sid] += data
    return m_dict


""" monomial op
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

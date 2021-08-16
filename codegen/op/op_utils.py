from fractions import Fraction

from ..sign_utils import \
    infer_negative_sign, infer_abs_sign, OpSign, insert_sign
from ..type_utils import \
    One, Zero, MinusOne, FloatTypes, validate_exp
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op

""" validate functions
"""
def num_valid_func(num_deps):
    def wrapper(*deps):
        assert len(deps) == num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
    return wrapper

""" equivalent functions
"""
def sequential_equiv_func(op_type, *deps):
    ids_str = ",".join([str(dep.id) for dep in deps])
    s = "{}:[{}]".format(op_type, ids_str)
    return s

def swappable_equiv_func(op_type, *deps):
    ids = [dep.id for dep in deps]
    sorted_ids = sorted(ids)
    ids_str = ",".join([str(op_id) for op_id in sorted_ids])
    s = "{}:[{}]".format(op_type, ids_str)
    return s

def mial_equiv_func(op_type, *deps):
    arr = []
    for i in range(1, len(deps), 2):
        dep, scalar = deps[i:i+2]
        dep_id = dep.id
        scalar_id = scalar.id
        arr.append((dep_id, scalar_id))
    narr = sorted(arr, key=lambda x: x[0])
    scalar = deps[0]
    scalar_id = scalar.id
    ids = [scalar_id]
    for op_id, scalar_id in narr:
        ids.append(op_id)
        ids.append(scalar_id)
    ids_str = ",".join([str(op_id) for op_id in ids])
    s = "{}:[{}]".format(op_type, ids_str)
    return s

""" sort functions
"""
def mial_sort_deps(*deps):
    arr = []
    for i in range(1, len(deps), 2):
        dep, scalar = deps[i:i+2]
        arr.append((dep, scalar))
    sorted_arr = sorted(arr, key=lambda x:x[0])
    scalar = deps[0]
    ndeps = [scalar]
    for dep, scalar in sorted_arr:
        ndeps.append(dep)
        ndeps.append(scalar)
    return ndeps

def swap_sort_deps(*deps):
    assert len(deps) == 2, len(deps)
    x, y = deps
    if x < y:
        ndeps = [x, y]
    else:
        ndeps = [y, x]
    return ndeps

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

""" monomial util functions
"""
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

""" polynomial util functions
"""
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
        var, coef = deps[:2]
        coef_data = coef.data
        var_id = var.id
        m_dict = {-1: coef_data, var_id: One}
        op = create_monomial_op(m_dict)
        return op
    scalar = od.scalar(scalar_data)
    deps = [scalar] + deps
    op = od.polynomial(*deps)
    return op

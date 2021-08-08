from fractions import Fraction
from codegen.infer_utils import \
    infer_power_sign, infer_multiply_sign, infer_add_sign
from codegen.op_utils import \
    One, Zero, FloatTypes, validate_exp, sequential_equiv_func
from codegen.op_def import Op, OpDef as od, Scalar

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
            assert isinstance(dep, Scalar), \
                "invalid type of dep: {}".format(type(dep))
            data = dep.data
            assert isinstance(data, FloatTypes), \
                "invalid type of data: {}".format(type(data))
        if mial_type == "poly" or num_deps == 1:
            return
        for i in range(2, num_deps, 2):
            dep = deps[i]
            data = dep.data
            assert isinstance(data, Fraction), \
                "invalid type of data: {}".format(type(data))

    return wrapper


""" mial ops
"""
@od.register_op(
    valid_func=mial_valid_func("mono"), equiv_func=sequential_equiv_func)
class Monomial(Op):
    def infer_sign(self):
        dep_signs = [dep.get_sign() for dep in self.deps]
        signs = [dep_signs[0]]
        for i in range(1, len(dep_signs), 2):
            frac_sign = dep_signs[i]
            exp = self.deps[i+1]
            exp_data = exp.data
            sign = infer_power_sign(frac_sign, exp_data)
            signs.append(sign)
        sign = signs[0]
        for cur_sign in signs[1:]:
            sign = infer_multiply_sign(sign, cur_sign)
        return sign

    @classmethod
    def fwd_func(cls, *v):
        product = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                product *= v[i]
            else:
                product *= v[i]**v[i+1]
        return product

    def autograph_backward(self, var_seq):
        assert len(self.deps) > 1, "invoke degenerate first"
        scalar = self.deps[0]
        scalar_data = scalar.data
        assert scalar_data != Zero, "invoke degenerate first"
        if len(self.deps) == 3 and scalar_data == One:
            exp = self.deps[2]
            exp_data = exp.data
            deno, nume = exp_data.denominator, exp_data.numerator
            assert deno != 1 or nume != 1, "invoke degenerate first"
        # create m_dict_ref
        m_dict_ref = {-1: scalar_data}
        for i in range(1, len(self.deps), 2):
            frac, exp = self.deps[i:i+2]
            exp_data = exp.data
            nume = exp_data.numerator
            assert nume != 0, "invoke degenerate first"
            assert not isinstance(frac, Scalar), "invoke degenerate first"
            frac_id = frac.id
            m_dict_ref[frac_id] = exp_data
        validate_monomial_dict(m_dict_ref)
        # create m_dict_dict
        m_dict_dict = {}
        for op_id, exp_data in m_dict_ref.items():
            if op_id == -1:
                continue
            m_dict = m_dict_ref.copy()
            deno, nume = exp_data.denominator, exp_data.numerator
            if deno == 1 and nume == 1:
                del m_dict[op_id]
            else:
                scalar_data = m_dict[-1]
                m_dict[-1] = scalar_data * exp_data
                nexp_data = exp_data - One
                m_dict[op_id] = nexp_data
            m_dict_dict[op_id] = m_dict.copy()
        # differentials
        diff_map = {}
        for i in range(1, len(self.deps), 2):
            dep = self.deps[i]
            dep_id = dep.id
            diff = dep.diff
            diff_map[dep_id] = diff
        ns = {len(diff) for diff in diff_map.values()}
        assert len(ns) == 1, ns
        n = list(ns)[0]
        # backward propagation
        self.diff.clear()
        op_ids = list(diff_map.keys())
        for i in range(n):
            m_dict = {-1: Zero}
            for op_id in op_ids:
                m_dict_partial = m_dict_dict[op_id]
                diff_in = diff_map[op_id]
                di = diff_in[i]
                m_dict_diff = get_monomial_dict(di)
                m_dict_in = merge_monomial_dict(
                    m_dict_partial, m_dict_diff)
                var = create_monomial_op(m_dict_in)
                var_id = var.id
                m_dict[var_id] = One
            diff = create_polynomial_op(m_dict)
            self.diff.append(diff)

    @classmethod
    def topo_degenerate(cls, *deps):
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


@od.register_op(
    valid_func=mial_valid_func("poly"), equiv_func=sequential_equiv_func)
class Polynomial(Op):
    def infer_sign(self):
        dep_signs = [dep.get_sign() for dep in self.deps]
        signs = [dep_signs[0]]
        for i in range(1, len(dep_signs), 2):
            x_sign, y_sign = dep_signs[i:i+2]
            sign = infer_multiply_sign(x_sign, y_sign)
            signs.append(sign)
        sign = signs[0]
        for cur_sign in signs[1:]:
            sign = infer_add_sign(sign, cur_sign)
        return sign

    @classmethod
    def fwd_func(cls, *v):
        summation = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                summation += v[i]
            else:
                summation += v[i]*v[i+1]
        return summation

    def autograph_backward(self, var_seq):
        assert len(self.deps) > 1, "invoke degenerate first"
        scalar = self.deps[0]
        scalar_data = scalar.data
        if len(self.deps) == 3 and scalar_data == Zero:
            coef = self.deps[2]
            coef_data = coef.data
            assert coef_data != One, "invoke degenerate first"
        # create m_dict_ref
        m_dict_ref = {-1: scalar_data}
        for i in range(1, len(self.deps), 2):
            var, coef = self.deps[i:i+2]
            coef_data = coef.data
            assert coef_data != Zero, "invoke degenerate first"
            assert not isinstance(var, Scalar), "invoke degenerate first"
            var_id = var.id
            m_dict_ref[var_id] = coef_data
        validate_polynomial_dict(m_dict_ref)
        # differentials
        diff_map = {}
        for i in range(1, len(self.deps), 2):
            dep = self.deps[i]
            dep_id = dep.id
            diff = dep.diff
            diff_map[dep_id] = diff
        ns = {len(diff) for diff in diff_map.values()}
        assert len(ns) == 1, ns
        n = list(ns)[0]
        # backward propagation
        self.diff.clear()
        op_ids = list(diff_map.keys())
        for i in range(n):
            m_dict = {-1: Zero}
            for op_id in op_ids:
                diff_in = diff_map[op_id]
                di = diff_in[i]
                did = di.id
                scalar_data = m_dict_ref[op_id]
                m_dict[did] = scalar_data
            diff = create_polynomial_op(m_dict)
            self.diff.append(diff)

    @classmethod
    def topo_degenerate(cls, *deps):
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

""" mial util functions
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
    if isinstance(op, Monomial):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            frac, exp = op.deps[i:i+2]
            exp_data = exp.data
            frac_id = frac.id
            m_dict[frac_id] = exp_data
    elif isinstance(op, Scalar):
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
        if isinstance(frac, Scalar):
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

def validate_polynomial_dict(m_dict):
    assert isinstance(m_dict, dict) and -1 in m_dict, m_dict
    for op_id, coef_data in m_dict.items():
        assert isinstance(op_id, int), type(op_id)
        assert isinstance(coef_data, FloatTypes), type(coef_data)
        if op_id == -1:
            continue
        frac = od.get_op(op_id)

def get_polynomial_dict(op):
    if isinstance(op, Polynomial):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            var, coef = op.deps[i:i+2]
            coef_data = coef.data
            var_id = var.id
            m_dict[var_id] = coef_data
    elif isinstance(op, Scalar):
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
        if isinstance(var, Scalar):
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

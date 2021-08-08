from fractions import Fraction

import numpy as np

from codegen.infer_utils import \
    infer_negative_sign, infer_nomorethan_sign, infer_abs_sign, \
    infer_add_sign, infer_power_sign, infer_mutual_sign, \
    infer_lessthan_sign, infer_multiply_sign
from codegen.op_utils import \
    One, MinusOne, Zero, validate_exp, \
    sequential_equiv_func, swappable_equiv_func
from codegen.op_def import Op, OpDef as od, Scalar
from codegen.mials import \
    create_monomial_op, get_monomial_dict, merge_monomial_dict, \
    get_polynomial_dict, merge_polynomial_dict

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

def power_valid_func(*deps):
    validate_num_deps = num_valid_func(2)
    validate_num_deps(*deps)
    exp = deps[1]
    assert isinstance(exp, Scalar), \
        "type of deps[1]: {} must be scalar".format(type(exp))


""" error definition
"""
class AssertExceedZeroError(Exception):
    pass


class AssertNotZeroError(Exception):
    pass


class AssertNoLessThanZeroError(Exception):
    pass


class ExpContradictError(Exception):
    pass


""" ops
"""
@od.register_opt("topo_fuse")
@od.register_opt("topo_standardize")
@od.register_op()
class Var(Op):
    @classmethod
    def topo_degenerate(cls, *deps):
        return cls.default_op(*deps)

    def forward(self):
        pass

    def autograph_backward(self, var_seq):
        self.diff = [od.scalar(Zero)] * len(var_seq)
        self.diff[var_seq[self.id]] = od.scalar(One)


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertExceedZero(Op):
    @classmethod
    def fwd_func(cls, v):
        # TODO: unittest test_assert_exceed_zero.py
        if v <= Zero:
            raise AssertExceedZeroError("value: {}".format(v))
        return Zero


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNotZero(Op):
    @classmethod
    def fwd_func(cls, v):
        # TODO: unittest test_assert_not_zero.py
        if v == Zero:
            raise AssertNotZeroError("value: {}".format(v))
        return Zero


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNoLessThanZero(Op):
    @classmethod
    def fwd_func(cls, v):
        if v < Zero:
            raise AssertNoLessThanZeroError("value: {}".format(v))
        return Zero


@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func = lambda v: -v

    def infer_sign(self):
        dep_sign = self.deps[0].get_sign()
        sign = infer_negative_sign(dep_sign)
        return sign

    @classmethod
    def topo_standardize(cls, deps):
        minus_one = od.scalar(-1)
        x = deps
        op = od.multiply(x, minus_one)
        return op


@od.register_opt("topo_fuse")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func = lambda v: np.sin(v)

    def autograph_backward(self, var_seq):
        x = self.deps[0]
        y = od.cos(x)
        self.diff.clear()
        for di in x.diff:
            dop = od.multiply(y, di)
            self.diff.append(dop)


@od.register_opt("topo_standardize")
@od.register_opt("topo_degenerate")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func = lambda v: np.abs(v)

    def infer_sign(self):
        dep_sign = self.deps[0].get_sign()
        sign = infer_abs_sign(dep_sign)
        return sign

    def autograph_backward(self, var_seq):
        x = self.deps[0]
        self.diff.clear()
        for di in x.diff:
            zero_op = od.scalar(0)
            neg_op = od.negative(di)
            dop = od.lessthan(di, zero_op, neg_op, di)
            self.diff.append(dop)


@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func = lambda v: np.cos(v)


@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Add(Op):
    fwd_func = lambda v0, v1: v0 + v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_add_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_fuse(cls, *deps):
        x, y = deps
        x_dict = get_polynomial_dict(x)
        y_dict = get_polynomial_dict(y)
        m_dict = merge_polynomial_dict(x_dict, y_dict)
        scalar = od.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = od.get_op(op_id)
            ndeps.append(dep)
            coef = od.scalar(scalar_data)
            ndeps.append(coef)
        if len(ndeps) == 3 and ndeps[0].data == Zero and \
            ndeps[2].data == One:
            return ndeps[1]
        op = od.polynomial(*ndeps)
        return op

    @classmethod
    def topo_degenerate(cls, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero:
            return y
        if isinstance(y, Scalar) and y.data == Zero:
            return x
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq):
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(var_seq)):
            dop = od.add(d0[i], d1[i])
            self.diff.append(dop)


@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func = lambda v0, v1: v0 - v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_negative_sign(y_sign)
        sign = infer_add_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        minus_y = od.multiply(minus_one, y)
        op = od.add(x, minus_y)
        return op


@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=num_valid_func(2), equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func = lambda v0, v1: v0 * v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_multiply_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_fuse(cls, *deps):
        x, y = deps
        x_dict = get_monomial_dict(x)
        y_dict = get_monomial_dict(y)
        m_dict = merge_monomial_dict(x_dict, y_dict)
        scalar = od.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = od.get_op(op_id)
            ndeps.append(dep)
            exp = od.scalar(scalar_data)
            ndeps.append(exp)
        if len(ndeps) == 3 and ndeps[0].data == One and \
            ndeps[2].data == One:
            return ndeps[1]
        op = od.monomial(*ndeps)
        return op

    @classmethod
    def topo_degenerate(cls, *deps):
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero or \
            isinstance(y, Scalar) and y.data == Zero:
            return od.scalar(0)
        if isinstance(x, Scalar) and x.data == One:
            return y
        if isinstance(y, Scalar) and y.data == One:
            return x
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq):
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(d0)):
            mul0 = od.multiply(x1, d0[i])
            mul1 = od.multiply(x0, d1[i])
            dop = od.add(mul0, mul1)
            self.diff.append(dop)


@od.register_opt("topo_standardize")
@od.register_op(
    valid_func=power_valid_func, equiv_func=sequential_equiv_func)
class Power(Op):
    fwd_func = lambda v0, v1: v0**v1

    def infer_sign(self):
        frac, exp = self.deps
        exp_data = exp.data
        frac_sign = frac.get_sign()
        sign = infer_power_sign(frac_sign, exp_data)
        return sign

    @classmethod
    def topo_fuse(cls, *deps):
        frac, exp = deps
        exp_data = exp.data
        assert isinstance(exp_data, Fraction) and \
            exp_data != One and exp_data != Zero, \
            "invalid exp_data: {}, run degenerate first".format(exp_data)
        m_dict = get_monomial_dict(frac)
        if len(m_dict) == 1:
            data = m_dict[-1]
            validate_exp(data, exp_data)
            scalar_data = data ** exp_data
            scalar = od.scalar(scalar_data)
            return scalar
        assert m_dict[-1] != Zero, m_dict[-1]
        deno, nume = exp_data.denominator, exp_data.numerator
        if nume < 0:
            # in case that: exp < 0
            # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
            # for op with op_id as i_n
            # validate op is not equal to zero
            for op_id, data in m_dict.items():
                if op_id == -1:
                    assert data != Zero
                    continue
                assert isinstance(data, Fraction)
                deno_in, nume_in = data.denominator, data.numerator
                if nume_in >= 0:
                    continue
                op = od.get_op(op_id)
                od.assertnotzero(op)
        if deno % 2 == 0 or deno > 1:
            # in case that: exp = Fraction(odd_num, even_num)
            # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
            # m_dict will first be decomposed into nm_dict and sm_dict
            # where
            # nm_dict: {-1: abs(scalar_data), j1: f1, j2: f2, ... }
            # where fn = Fraction(odd_num, even_num) or 
            #            Fraction(even_num, odd_num) or
            #            Fraction(odd_num1, odd_num2>1)
            # sm_dict: {-1, sgn(scalar_data), k1: g1, k2: g2, ... }
            # where gn = Fraction(odd_num, 1)
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
                cop = od.get_op(op_id)
                if nume_in % 2 == 0 or deno_in % 2 == 0 or \
                    isinstance(cop, Abs) or deno_in > 1:
                    if nume_in % 2 == 0:
                        assert not isinstance(cop, Abs), type(cop)
                    nm_dict[op_id] = data
                    continue
                sm_dict[op_id] = data
            if len(sm_dict) > 1:
                od.assertexceedzero(frac)
                frac_id = frac.id
                m_dict = {-1: One, frac_id: One}
            else:
                assert -1 in sm_dict, sm_dict.keys()
                scalar_data = sm_dict[-1]
                if scalar_data < Zero:
                    # unittest test_power_3.py
                    raise ExpContradictError(
                        "contradictory exp_data: {}, ".format(exp_data) + \
                        "dep_ids: {}".format([dep.id for dep in deps]))
                m_dict = nm_dict
        # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
        # exp = Fraction(nume, deno)
        # in case that en = Fraction(even_num, odd_num)
        # and deno = k * even_num
        # the op with op_id as i_n should be turned into abs(op)
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                continue
            assert isinstance(data, Fraction), type(data)
            nume_in = data.numerator
            if nume_in % 2 == 0 and deno >= nume_in and \
                deno % nume_in == 0:
                del m_dict[op_id]
                cop = od.get_op(op_id)
                nop = od.abs(cop)
                nid = nop.id
                if nid not in m_dict:
                    m_dict[nid] = data
                else:
                    # unittest test_power_2.py
                    m_dict[nid] += data
                    assert isinstance(m_dict[nid], Fraction), type(m_dict[nid])
        # update m_dict by power
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                scalar_data = data ** exp_data
                m_dict[-1] = scalar_data
                continue
            ndata = data * exp_data
            assert isinstance(ndata, Fraction), type(ndata)
            m_dict[op_id] = ndata
        # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
        # in case that en = Fraction(2*k, deno_in)
        # and op with op_id i_n is abs(sop)
        # then op should be turned into sop
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                continue
            assert isinstance(data, Fraction), type(data)
            nume_in = data.numerator
            op = od.get_op(op_id)
            if nume_in % 2 == 0 and isinstance(op, Abs):
                del m_dict[op_id]
                sop = op.deps[0]
                sid = sop.id
                if sid not in m_dict:
                    m_dict[sid] = data
                else:
                    # unittest test_power_1.py
                    m_dict[sid] += data
                    assert isinstance(m_dict[sid], Fraction), type(m_dict[sid])
        # create monomial op
        op = create_monomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, *deps):
        frac, exp = deps
        exp_data = exp.data
        assert isinstance(exp_data, Fraction), type(exp_data)
        nume = exp_data.numerator
        if nume == 0:
            op = od.scalar(One)
            return op
        if isinstance(frac, Scalar):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            scalar_data = frac_data ** exp_data
            op = od.scalar(scalar_data)
            return op
        deno = exp_data.denominator
        if deno == 1 and nume == 1:
            return frac
        return cls.default_op(*deps)

    def autograph_backward(self, var_seq):
        x, y = self.deps
        d = x.diff
        assert any([dd is not None for dd in d])
        self.diff.clear()
        nscalar = od.scalar(y.data-1)
        npower = od.power(x, nscalar)
        mul_scalar = od.multiply(y, npower)
        for i in range(len(d)):
            dop = od.multiply(mul_scalar, d[i])
            self.diff.append(dop)


@od.register_op(
    valid_func=num_valid_func(2), equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func = lambda v0, v1: v0 / v1

    def infer_sign(self):
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_power_sign(y_sign, MinusOne)
        sign = infer_multiply_sign(x_sign, ny_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps):
        x, y = deps
        minus_one = od.scalar(-1)
        _pow = od.power(y, minus_one)
        op = od.multiply(x, _pow)
        return op

def cnd_auto_backward(deps, od_func, var_seq):
    lhs, rhs, lv, rv = deps
    dl, dr = lv.diff, rv.diff
    diff = []
    for i in range(len(dl)):
        dop = od_func(lhs, rhs, dl[i], dr[i])
        diff.append(dop)
    return diff


@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def infer_sign(self):
        a_sign, b_sign, x_sign, y_sign = \
            [dep.get_sign() for dep in self.deps]
        if infer_lessthan_sign(a_sign, b_sign):
            return x_sign
        if infer_nomorethan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq):
        od_func = getattr(od, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)


@od.register_opt("topo_standardize")
@od.register_opt("topo_fuse")
@od.register_op(
    valid_func=num_valid_func(4), equiv_func=sequential_equiv_func)
class NoMoreThan(Op):
    fwd_func = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    def infer_sign(self):
        a_sign, b_sign, x_sign, y_sign = \
            [dep.get_sign() for dep in self.deps]
        if infer_nomorethan_sign(a_sign, b_sign):
            return x_sign
        if infer_lessthan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps):
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq):
        od_func = getattr(od, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)

from fractions import Fraction

from codegen.infer_utils import infer_power_sign
from codegen.op_utils import \
    One, MinusOne, Zero, validate_exp, sequential_equiv_func
from codegen.op_def import Op, OpDef as od
from codegen.mials import create_monomial_op, get_monomial_dict
from codegen.ops import num_valid_func

""" validate functions
"""
def power_valid_func(*deps):
    validate_num_deps = num_valid_func(2)
    validate_num_deps(*deps)
    exp = deps[1]
    assert isinstance(exp, od.get_op_cls("scalar")), \
        "type of deps[1]: {} must be scalar".format(type(exp))


""" error definition
"""
class ExpContradictError(Exception):
    pass


""" power op
"""
@od.register_opt("dfs_forward")
@od.register_opt("dfs_info")
@od.register_opt("dfs_tosym")
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
                    isinstance(cop, od.get_op_cls("abs")) or deno_in > 1:
                    if nume_in % 2 == 0:
                        assert not isinstance(cop, od.get_op_cls("abs")), \
                            type(cop)
                    nm_dict[op_id] = data
                    continue
                sm_dict[op_id] = data
            if len(sm_dict) > 1:
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
            if nume_in % 2 == 0 and isinstance(op, od.get_op_cls("abs")):
                del m_dict[op_id]
                sop = op.deps[0]
                sid = sop.id
                if sid not in m_dict:
                    m_dict[sid] = data
                else:
                    # unittest test_power_1.py
                    m_dict[sid] += data
                    assert isinstance(
                        m_dict[sid], Fraction), type(m_dict[sid])
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
        if isinstance(frac, od.get_op_cls("scalar")):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            scalar_data = frac_data ** exp_data
            op = od.scalar(scalar_data)
            return op
        deno = exp_data.denominator
        if deno == 1 and nume == 1:
            return frac
        return cls.default_op(*deps)

    def dfs_autograph_backward(self, diff_dict, var_seq):
        cop_id = self.id
        assert cop_id not in diff_dict
        x, y = self.deps
        xid = x.id
        xdiff = diff_dict[xid]
        assert any([dd is not None for dd in xdiff])
        nscalar = od.scalar(y.data-1)
        npower = od.power(x, nscalar)
        mul_scalar = od.multiply(y, npower)
        cdiff = []
        for i in range(len(xdiff)):
            dop = od.multiply(mul_scalar, xdiff[i])
            cdiff.append(dop)
        diff_dict[cop_id] = cdiff

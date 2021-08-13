from fractions import Fraction

from codegen.sign_utils import \
    infer_power_sign, merge_sign, revinfer_power_sign, OpSign
from codegen.op_utils import \
    One, MinusOne, Zero, validate_exp, ContradictError, \
    sequential_equiv_func
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op
from codegen.mials import create_monomial_op, get_monomial_dict
from codegen.ops import num_valid_func

""" validate functions
"""
def power_valid_func(*deps):
    validate_num_deps = num_valid_func(2)
    validate_num_deps(*deps)
    exp = deps[1]
    assert isinstance(exp, org.get_op_cls("scalar")), \
        "type of deps[1]: {} must be scalar".format(type(exp))


""" power op
"""
@org.register_opt("dfs_forward")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("topo_standardize")
@org.register_op(
    valid_func=power_valid_func, equiv_func=sequential_equiv_func)
class Power(Op):
    fwd_func = lambda v0, v1: v0**v1

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        frac, exp = deps
        exp_data = exp.data
        deno, nume = exp_data.denominator, exp_data.numerator
        if deno == 1 and nume == 1:
            return frac
        if nume == 0:
            op = od.scalar(One)
            return op
        m_dict = get_monomial_dict(frac)
        if len(m_dict) == 1 or m_dict[-1] == Zero:
            data = m_dict[-1]
            validate_exp(data, exp_data)
            scalar_data = data ** exp_data
            scalar = od.scalar(scalar_data)
            return scalar
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
                    nm_dict[op_id] += One
                else:
                    nm_dict[op_id] = One
            m_dict = nm_dict
        nm_dict = m_dict.copy()
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
                else:
                    nop = od.abs(cop)
                del m_dict[op_id]
                nid = nop.id
                if nid not in m_dict:
                    m_dict[nid] = data
                else:
                    # unittest test_power_2.py
                    m_dict[nid] += data
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                scalar_data = data ** exp_data
                m_dict[-1] = scalar_data
                continue
            ndata = data * exp_data
            m_dict[op_id] = ndata
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
        # create monomial op
        op = create_monomial_op(m_dict)
        return op

    def revtopo_infer_sign(self, sign_dict):
        frac, exp = self.deps
        frac_id = frac.id
        exp_data = exp.data
        cop_id = self.id
        csign = sign_dict[cop_id]
        frac_sign = sign_dict[frac_id]
        sign = revinfer_power_sign(csign, exp_data)
        frac_sign = merge_sign(frac_sign, sign)
        sign_dict[frac_id] = frac_sign

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        frac, exp = deps
        exp_data = exp.data
        assert isinstance(exp_data, Fraction), type(exp_data)
        nume = exp_data.numerator
        if nume == 0:
            op = od.scalar(One)
            return op
        if isinstance(frac, org.get_op_cls("scalar")):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            scalar_data = frac_data ** exp_data
            op = od.scalar(scalar_data)
            return op
        deno = exp_data.denominator
        if deno == 1:
            if nume == 1:
                return frac
            if nume % 2 == 0 and isinstance(frac, org.get_op_cls("abs")):
                dep = frac.deps[0]
                return cls.default_op(dep, exp)
        return cls.default_op(*deps)

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        x, y = self.deps
        xid = x.id
        xdiff = val_dict[xid]
        assert any([dd is not None for dd in xdiff])
        nscalar = od.scalar(y.data-1)
        npower = od.power(x, nscalar)
        mul_scalar = od.multiply(y, npower)
        cdiff = []
        for i in range(len(xdiff)):
            dop = od.multiply(mul_scalar, xdiff[i])
            cdiff.append(dop)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        frac, exp = self.deps
        frac_id = frac.id
        frac_sign = val_dict[frac_id]
        exp_data = exp.data
        sign = infer_power_sign(frac_sign, exp_data)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

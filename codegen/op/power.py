from fractions import Fraction

from ..utils.sign_utils import \
    infer_power_sign, merge_sign, revinfer_power_sign, OpSign
from ..utils.type_utils import \
    One, MinusOne, Zero, validate_exp, ContradictError
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import \
    num_valid_func, sequential_equiv_func, \
    create_monomial_op, get_monomial_dict, get_monomial_dict_exp

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
@org.register_opt("dfs_sort_deps")
@org.register_opt("topo_standardize")
@org.register_op(
    valid_func=power_valid_func, equiv_func=sequential_equiv_func)
class Power(Op):
    fwd_func = lambda v0, v1: v0**v1

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        frac, exp = deps
        exp_data = exp.data
        m_dict = get_monomial_dict_exp(frac, exp_data, sign_dict)
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

    def dfs_autodiff(self, val_dict, var_seq):
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

from enum import Enum, auto
from fractions import Fraction

from codegen.op_utils import ContradictError


""" Op Sign Types
"""
class OpSign(Enum):
    NON_NEGATIVE = auto()
    POSITIVE = auto()
    NON_POSITIVE = auto()
    NEGATIVE = auto()
    NON_ZERO = auto()
    ZERO = auto()
    INDEFINITE = auto()

""" infer_sign util functions
"""
def infer_negative_sign(sign):
    if sign == OpSign.POSITIVE:
        return OpSign.NEGATIVE
    if sign == OpSign.NEGATIVE:
        return OpSign.POSITIVE
    if sign == OpSign.NON_NEGATIVE:
        return OpSign.NON_POSITIVE
    if sign == OpSign.NON_POSITIVE:
        return OpSign.NON_NEGATIVE
    return sign

def infer_add_sign(x_sign, y_sign):
    lst1 = [OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    lst2 = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
    if x_sign == OpSign.INDEFINITE or y_sign == OpSign.INDEFINITE:
        return OpSign.INDEFINITE
    if x_sign == OpSign.ZERO:
        return y_sign
    if y_sign == OpSign.ZERO:
        return x_sign
    if x_sign == OpSign.NON_ZERO or y_sign == OpSign.NON_ZERO:
        return OpSign.INDEFINITE
    if x_sign == y_sign:
        return x_sign
    if x_sign in lst1 and y_sign in lst1:
        return OpSign.POSITIVE
    if x_sign in lst2 and y_sign in lst2:
        return OpSign.NEGATIVE
    return OpSign.INDEFINITE

def infer_abs_sign(sign):
    if sign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
        return OpSign.POSITIVE
    return OpSign.NON_NEGATIVE

def infer_multiply_sign(x_sign, y_sign):
    if x_sign == OpSign.ZERO or y_sign == OpSign.ZERO:
        return OpSign.ZERO
    if x_sign == OpSign.INDEFINITE or y_sign == OpSign.INDEFINITE:
        return OpSign.INDEFINITE
    lst1 = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
    if x_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if y_sign in lst1 else OpSign.INDEFINITE
        return sign
    if y_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if x_sign in lst1 else OpSign.INDEFINITE
        return sign
    lst2 = [OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    if x_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if y_sign in lst2 else OpSign.NON_POSITIVE
        return sign
    if y_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if x_sign in lst2 else OpSign.NON_POSITIVE
        return sign
    if x_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if y_sign in lst2 else OpSign.NON_NEGATIVE
        return sign
    if y_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if x_sign in lst2 else OpSign.NON_NEGATIVE
        return sign
    sign = OpSign.POSITIVE if x_sign == y_sign else OpSign.NEGATIVE
    return sign

def infer_power_sign(frac_sign, exp_data):
    assert isinstance(exp_data, Fraction), type(exp_data)
    nume, deno = exp_data.numerator, exp_data.denominator
    if nume < 0:
        assert frac_sign != OpSign.ZERO
    if deno > 1:
        assert frac_sign != OpSign.NEGATIVE
    if nume == 0:
        return OpSign.POSITIVE
    if nume % 2 == 0 or deno > 1:
        sign = infer_abs_sign(frac_sign) if nume > 0 else OpSign.POSITIVE
        return sign
    if frac_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if nume > 0 else OpSign.POSITIVE
        return sign
    if frac_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if nume > 0 else OpSign.NEGATIVE
        return sign
    return frac_sign

def infer_mutual_sign(x_sign, y_sign):
    if x_sign == y_sign:
        return x_sign
    lst1 = [OpSign.ZERO, OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    if x_sign in lst1 and y_sign in lst1:
        return OpSign.NON_NEGATIVE
    lst2 = [OpSign.ZERO, OpSign.NEGATIVE, OpSign.NON_POSITIVE]
    if x_sign in lst2 and y_sign in lst2:
        return OpSign.NON_POSITIVE
    lst3 = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
    if x_sign in lst3 and y_sign in lst3:
        return OpSign.NON_ZERO
    return OpSign.INDEFINITE

def infer_lessthan_sign(a_sign, b_sign):
    if a_sign == OpSign.NEGATIVE and \
        b_sign in [Opsign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    if a_sign in [OpSign.NON_POSITIVE, OpSign.ZERO] and \
        b_sign == OpSign.POSITIVE:
        return True
    return False

def infer_nomorethan_sign(a_sign, b_sign):
    if a_sign in [OpSign.ZERO, OpSign.NON_POSITIVE, OpSign.NEGATIVE] and \
        b_sign in [OpSign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    return False

""" assertion util functions
"""
def raise_sign_assertion_error(sign1, sign2):
    raise ContradictError(
        "contradictory assertion, sign1: {}, sign2: {}".format(
            sign1, sign2))

def merge_assertion(sign1, sign2):
    if sign1 == sign2:
        return sign1
    if sign1 == OpSign.INDEFINITE:
        return sign2
    if sign2 == OpSign.INDEFINITE:
        return sign1
    if sign1 == OpSign.NON_ZERO:
        if sign2 == OpSign.ZERO:
            raise_sign_assertion_error(sign1, sign2)
        return sign2
    if sign2 == OpSign.NON_ZERO:
        if sign1 == OpSign.ZERO:
            raise_sign_assertion_error(sign1, sign2)
        return sign1
    if sign1 == OpSign.NON_POSITIVE:
        if sign2 == OpSign.POSITIVE:
            raise_sign_assertion_error(sign1, sign2)
        if sign2 == OpSign.NEGATIVE:
            return sign2
        return OpSign.ZERO
    if sign2 == OpSign.NON_POSITIVE:
        if sign1 == OpSign.POSITIVE:
            raise_sign_assertion_error(sign1, sign2)
        if sign1 == OpSign.NEGATIVE:
            return sign1
        return OpSign.ZERO
    if sign1 == OpSign.NON_NEGATIVE:
        if sign2 == OpSign.NEGATIVE:
            raise_sign_assertion_error(sign1, sign2)
        if sign2 == OpSign.POSITIVE:
            return sign2
        return OpSign.ZERO
    if sign2 == OpSign.NON_NEGATIVE:
        if sign1 == OpSign.NEGATIVE:
            raise_sign_assertion_error(sign1, sign2)
        if sign1 == OpSign.POSITIVE:
            return sign1
        return OpSign.ZERO
    raise_sign_assertion_error(sign1, sign2)

def merge_assertions(signs):
    if len(signs) == 0:
        return []
    csign = signs[0]
    for sign in signs[1:]:
        csign = merge_assertion(csign, sign)
    if csign == OpSign.INDEFINITE:
        return []
    return [csign]

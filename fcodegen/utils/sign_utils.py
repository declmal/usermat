from enum import Enum, auto
from fractions import Fraction

from .type_utils import ContradictError, Zero


""" Op Sign Types
"""
class OpSign(Enum):
    NON_NEGATIVE = auto()
    POSITIVE = auto()
    NON_POSITIVE = auto()
    NEGATIVE = auto()
    NON_ZERO = auto()
    ZERO = auto()
    UNDEFINED = auto()

""" merge sign util functions
"""
def raise_merge_sign_error(sign1, sign2):
    raise ContradictError(
        "contradictory assertion, sign1: {}, sign2: {}".format(
            sign1, sign2))

def merge_sign(sign1, sign2):
    if sign1 == sign2:
        return sign1
    if sign1 == OpSign.UNDEFINED:
        return sign2
    if sign2 == OpSign.UNDEFINED:
        return sign1
    if sign1 == OpSign.NON_ZERO:
        if sign2 == OpSign.ZERO:
            raise_merge_sign_error(sign1, sign2)
        if sign2 == OpSign.NON_NEGATIVE:
            return OpSign.POSITIVE
        if sign2 == OpSign.NON_POSITIVE:
            return OpSign.NEGATIVE
        return sign2
    if sign2 == OpSign.NON_ZERO:
        if sign1 == OpSign.ZERO:
            raise_merge_sign_error(sign1, sign2)
        if sign1 == OpSign.NON_NEGATIVE:
            return OpSign.POSITIVE
        if sign1 == OpSign.NON_POSITIVE:
            return OpSign.NEGATIVE
        return sign1
    if sign1 == OpSign.NON_POSITIVE:
        if sign2 == OpSign.POSITIVE:
            raise_merge_sign_error(sign1, sign2)
        if sign2 == OpSign.NEGATIVE:
            return sign2
        return OpSign.ZERO
    if sign2 == OpSign.NON_POSITIVE:
        if sign1 == OpSign.POSITIVE:
            raise_merge_sign_error(sign1, sign2)
        if sign1 == OpSign.NEGATIVE:
            return sign1
        return OpSign.ZERO
    if sign1 == OpSign.NON_NEGATIVE:
        if sign2 == OpSign.NEGATIVE:
            raise_merge_sign_error(sign1, sign2)
        if sign2 == OpSign.POSITIVE:
            return sign2
        return OpSign.ZERO
    if sign2 == OpSign.NON_NEGATIVE:
        if sign1 == OpSign.NEGATIVE:
            raise_merge_sign_error(sign1, sign2)
        if sign1 == OpSign.POSITIVE:
            return sign1
        return OpSign.ZERO
    raise_merge_sign_error(sign1, sign2)

def is_sub_sign(sign, sign1):
    try:
        merged_sign = merge_sign(sign, sign1)
    except ConnectionError:
        return False
    if merged_sign == sign:
        return True
    return False

def separate_signs(signs, lst):
    signs1 = []
    signs2 = []
    for sign in signs:
        if sign in lst:
            signs1.append(sign)
        else:
            signs2.append(sign)
    return signs1, signs2

""" infer_sign util functions
"""
def infer_scalar_sign(data):
    if data == Zero:
        sign = OpSign.ZERO
    elif data > Zero:
        sign = OpSign.POSITIVE
    else:
        sign = OpSign.NEGATIVE
    return sign

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
    if x_sign == OpSign.UNDEFINED or y_sign == OpSign.UNDEFINED:
        return OpSign.UNDEFINED
    if x_sign == OpSign.ZERO:
        return y_sign
    if y_sign == OpSign.ZERO:
        return x_sign
    if x_sign == OpSign.NON_ZERO or y_sign == OpSign.NON_ZERO:
        return OpSign.UNDEFINED
    if x_sign == y_sign:
        return x_sign
    if x_sign in lst1 and y_sign in lst1:
        return OpSign.POSITIVE
    if x_sign in lst2 and y_sign in lst2:
        return OpSign.NEGATIVE
    return OpSign.UNDEFINED

def infer_abs_sign(sign):
    if sign in [OpSign.POSITIVE, OpSign.NEGATIVE]:
        return OpSign.POSITIVE
    if sign == OpSign.ZERO:
        return OpSign.ZERO
    return OpSign.NON_NEGATIVE

def infer_multiply_sign(x_sign, y_sign):
    if x_sign == OpSign.ZERO or y_sign == OpSign.ZERO:
        return OpSign.ZERO
    if x_sign == OpSign.UNDEFINED or y_sign == OpSign.UNDEFINED:
        return OpSign.UNDEFINED
    lst1 = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
    if x_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if y_sign in lst1 else OpSign.UNDEFINED
        return sign
    if y_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if x_sign in lst1 else OpSign.UNDEFINED
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
    if nume < 0 and frac_sign == OpSign.ZERO:
        raise ContradictError
    if deno > 1 and frac_sign == OpSign.NEGATIVE:
        raise ContradictError
    if nume == 0:
        return OpSign.POSITIVE
    if nume % 2 == 0 or deno > 1:
        sign = infer_abs_sign(frac_sign) if nume > 0 else OpSign.POSITIVE
        return sign
    if nume == 1 and deno == 1:
        return frac_sign
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
    return OpSign.UNDEFINED

def infer_multiply_sign_consec(signs):
    assert len(signs) > 0
    csign = signs[0]
    for sign in signs[1:]:
        csign = infer_multiply_sign(csign, sign)
    return csign

def infer_add_sign_consec(signs):
    assert len(signs) > 0
    csign = signs[0]
    for sign in signs[1:]:
        csign = infer_add_sign(csign, sign)
    return csign

""" infer relation functions
"""
def infer_lessthan(a_sign, b_sign):
    if a_sign == OpSign.NEGATIVE and \
        b_sign in [Opsign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    if a_sign in [OpSign.NON_POSITIVE, OpSign.ZERO] and \
        b_sign == OpSign.POSITIVE:
        return True
    return False

def infer_nomorethan(a_sign, b_sign):
    if a_sign in [OpSign.ZERO, OpSign.NON_POSITIVE, OpSign.NEGATIVE] and \
        b_sign in [OpSign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    return False

def infer_notequal(a_sign, b_sign):
    if a_sign == OpSign.NEGATIVE and \
        b_sign in [OpSign.NON_NEGATIVE, OpSign.ZERO, OpSign.POSITIVE]:
        return True
    if a_sign == OpSign.NON_POSITIVE and b_sign == OpSign.POSITIVE:
        return True
    if a_sign == OpSign.POSITIVE and \
        b_sign in [OpSign.NON_POSITIVE, OpSign.ZERO, OpSign.NEGATIVE]:
        return True
    if a_sign == OpSign.NON_NEGATIVE and b_sign == OpSign.NEGATIVE:
        return True
    if a_sign == OpSign.NON_ZERO and b_sign == OpSign.ZERO:
        return True
    if a_sign == OpSign.ZERO and b_sign == OpSign.NON_ZERO:
        return True
    return False

""" rev infer util functions
"""
def revinfer_multiply_sign(csign, xsign):
    if csign == OpSign.UNDEFINED:
        return OpSign.UNDEFINED
    if csign == OpSign.NON_ZERO:
        return OpSign.NON_ZERO
    if csign == OpSign.ZERO:
        if xsign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
            return OpSign.ZERO
        return OpSign.UNDEFINED
    lst1 = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
    lst2 = [OpSign.NON_NEGATIVE, OpSign.POSITIVE]
    if csign == OpSign.NON_NEGATIVE:
        if xsign in lst1:
            return OpSign.NON_POSITIVE
        if xsign in lst2:
            return OpSign.NON_NEGATIVE
        return OpSign.UNDEFINED
    if csign == OpSign.NON_POSITIVE:
        if xsign in lst1:
            return OpSign.NON_NEGATIVE
        if xsign in lst2:
            return OpSign.NON_POSITIVE
        return OpSign.UNDEFINED
    if csign == OpSign.POSITIVE:
        if xsign == OpSign.NEGATIVE:
            return OpSign.NEGATIVE
        if xsign == OpSign.POSITIVE:
            return OpSign.POSITIVE
        return OpSign.NON_ZERO
    if csign == OpSign.NEGATIVE:
        if xsign == OpSign.NEGATIVE:
            return OpSign.POSITIVE
        if xsign == OpSign.POSITIVE:
            return OpSign.NEGATIVE
        return OpSign.NON_ZERO
    assert False

def revinfer_add_sign(csign, xsign):
    if csign == OpSign.UNDEFINED or xsign == OpSign.UNDEFINED:
        return OpSign.UNDEFINED
    if csign == OpSign.NON_ZERO:
        if xsign == OpSign.ZERO:
            return OpSign.NON_ZERO
        return OpSign.UNDEFINED
    if csign == OpSign.ZERO:
        if xsign == OpSign.ZERO:
            return OpSign.ZERO
        if xsign == OpSign.NON_NEGATIVE:
            return OpSign.NON_POSITIVE
        if xsign == OpSign.NON_POSITIVE:
            return OpSign.NON_NEGATIVE
        if xsign == OpSign.POSITIVE:
            return OpSign.NEGATIVE
        if xsign == OpSign.NEGATIVE:
            return OpSign.POSITIVE
        return OpSign.UNDEFINED
    if csign == OpSign.NON_NEGATIVE:
        if xsign == OpSign.NEGATIVE:
            return OpSign.POSITIVE
        if xsign == OpSign.NON_POSITIVE:
            return OpSign.NON_NEGATIVE
        return OpSign.UNDEFINED
    if csign == OpSign.NON_POSITIVE:
        if xsign == OpSign.POSITIVE:
            return OpSign.NEGATIVE
        if xsign == OpSign.NON_NEGATIVE:
            return OpSign.NON_POSITIVE
        return OpSign.UNDEFINED
    if csign == OpSign.POSITIVE:
        if xsign in [OpSign.NON_POSITIVE, OpSign.NEGATIVE]:
            return OpSign.POSITIVE
        return OpSign.UNDEFINED
    if csign == OpSign.NEGATIVE:
        if xsign in [OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
            return OpSign.NEGATIVE
        return OpSign.UNDEFINED
    assert False

def revinfer_power_sign(csign, exp_data):
    nume = exp_data.numerator
    if nume == 0:
        assert csign == OpSign.POSITIVE, csign
        return OpSign.UNDEFINED
    deno = exp_data.denominator
    if deno > 1 or nume % 2 == 0:
        if nume < 0:
            assert csign == OpSign.POSITIVE, csign
        else:
            assert csign in [ \
                OpSign.NON_NEGATIVE, OpSign.POSITIVE, OpSign.ZERO], csign
    if deno > 1:
        if nume < 0:
            frac_sign = OpSign.POSITIVE
        else:
            frac_sign = OpSign.NON_NEGATIVE
        return frac_sign
    if nume % 2 == 0:
        frac_sign = OpSign.UNDEFINED if nume > 0 else OpSign.NON_ZERO
        return frac_sign
    frac_sign = csign if nume > 0 else merge_sign(csign, OpSign.NON_ZERO)
    return frac_sign

""" sign dict util functions
"""
def insert_sign(op_id, sign_dict, sign):
    if op_id not in sign_dict:
        sign_dict[op_id] = sign
        return
    osign = sign_dict[op_id]
    csign = merge_sign(osign, sign)
    sign_dict[op_id] = csign

from typing import Dict, List, Callable, Optional, Union, Any, Set
from fractions import Fraction
from enum import Enum, auto

import numpy as np

""" Types
"""
Float = Union[
    "int", "np.int32", "np.int64", "float",
    "np.float32", "np.float64", "Fraction"]
FloatTypes = (
    int, np.int32, np.int64, float, np.float32, np.float64, Fraction)
GradFuncType = Callable[["np.float64"], "np.float64"]
FwdFuncType = Callable[[List["np.float64"]], "np.float64"]
EquivFuncType = Callable[[str, List["Op"]], List[str]]
OpEquivFuncType = Callable[[List["Op"]], List[str]]
ValidFuncType = Callable[[List["Op"]], None]

""" equivalent functions
"""
default_equiv_func: "EquivFuncType" = lambda op_type, ops: []
sequential_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join([str(op.id) for op in ops]))]
swappable_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join(
            sorted([str(op.id) for op in ops])))]

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

""" infer_sign functions
"""

def infer_negative_sign(sign: "OpSign") -> "OpSign":
    if sign == OpSign.POSITIVE:
        return OpSign.NEGATIVE
    if sign == OpSign.NEGATIVE:
        return OpSign.POSITIVE
    if sign == OpSign.NON_NEGATIVE:
        return OpSign.NON_POSITIVE
    if sign == OpSign.NON_POSITIVE:
        return OpSign.NON_NEGATIVE
    return sign

def infer_add_sign(x_sign: "OpSign", y_sign: "OpSign") -> "OpSign":
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

def infer_abs_sign(sign: "OpSign") -> "OpSign":
    if sign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
        return OpSign.POSITIVE
    return OpSign.NON_NEGATIVE

def infer_multiply_sign(x_sign: "OpSign", y_sign: "OpSign") -> "OpSign":
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

def infer_power_sign(frac_sign: "Opsign", exp_data: "Fraction") -> "OpSign":
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

def infer_mutual_sign(x_sign: "OpSign", y_sign: "Opsign") -> "OpSign":
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

def infer_lessthan_sign(a_sign: "OpSign", b_sign: "OpSign") -> bool:
    if a_sign == OpSign.NEGATIVE and \
        b_sign in [Opsign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    if a_sign in [OpSign.NON_POSITIVE, OpSign.ZERO] and b_sign == OpSign.POSITIVE:
        return True
    return False

def infer_nomorethan_sign(a_sign: "OpSign", b_sign: "OpSign") -> bool:
    if a_sign in [OpSign.ZERO, OpSign.NON_POSITIVE, OpSign.NEGATIVE] and \
        b_sign in [OpSign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    return False

""" scalar datas
"""

# Zero = np.float64(0.0)
# One = np.float64(1.0)
Zero = Fraction(0)
One = Fraction(1)
MinusOne = Fraction(-1)

""" cast functions
"""

def cast_float(scalar: "Float") -> "np.float64":
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)

def cast_fraction(scalar: "Float") -> "Fraction":
    if isinstance(scalar, Fraction):
        return scalar
    else:
        return Fraction(scalar)

from fractions import Fraction
import sys

import numpy as np

""" Types
"""
FloatTypes = (
    int, np.int32, np.int64, float, np.float32, np.float64, Fraction)

""" scalar datas
"""
Zero = Fraction(0)
One = Fraction(1)
MinusOne = Fraction(-1)
Half = Fraction(1, 2)
PositiveInf = float("inf")
NegativeInf = -PositiveInf
Epsilon = sys.float_info.min

def get_forward_eps(v):
    eps = 1.0
    while v + eps > v:
        eps /= 2
    if eps == Zero:
        eps = Epsilon
    else:
        eps *= 2
    return eps

def get_backward_eps(v):
    eps = 1.0
    while v - eps < v:
        eps /= 2
    if eps == Zero:
        eps = Epsilon
    else:
        eps *= 2
    return eps

def get_infsimal(v, forward=True):
    assert isinstance(v, FloatTypes), type(v)
    if forward:
        eps = get_forward_eps(v)
        nv = v + eps
        assert nv > v, \
            "eps: {}, nv: {}, v: {}".format(eps, nv ,v)
    else:
        eps = get_backward_eps(v)
        nv = v - eps
        assert nv < v, \
            "eps: {}, nv: {}, v: {}".format(eps, nv ,v)
    return nv

""" cast functions
"""
def cast_float(scalar):
    assert isinstance(scalar, FloatTypes)
    if isinstance(scalar, np.float64):
        return scalar
    return np.float64(scalar)

def cast_fraction(scalar):
    assert isinstance(scalar, FloatTypes)
    if isinstance(scalar, Fraction):
        return scalar
    return Fraction(scalar)

""" util functions
"""
def validate_exp(frac_data, exp_data):
    deno, nume = exp_data.denominator, exp_data.numerator
    if frac_data == Zero:
        assert nume >= Zero, \
            "zero division occurs: frac_data: {}".format(
                frac_data) + ", exp_data: {}".format(exp_data)
    elif frac_data < Zero:
        assert deno == 1, \
            "the denominator of exp must be one for negative fraction," + \
            " frac_data: {}, exp_data: {}".format(frac_data, exp_data)


""" error definition
"""
class ContradictError(Exception):
    pass

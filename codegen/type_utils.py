from fractions import Fraction

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

""" equivalent functions
"""
sequential_equiv_func = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join([str(op.id) for op in ops]))]
swappable_equiv_func = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join(
            sorted([str(op.id) for op in ops])))]

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

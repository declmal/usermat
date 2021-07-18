from typing import Dict, List, NamedTuple, Callable, Optional, Union

import numpy as np

Scalar = Union['int', 'float', 'np.float64']
GradFunc = Callable[['np.float64'], 'np.float64']

def cast_scalar(scalar: 'Scalar') -> 'np.float64':
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)


class Sym:
    grad_fns: List['GradFunc'] = []
    n_syms: int = 0

    def __init__(self, *syms: 'Sym', **params: 'Scalar') -> None:
        self._check_syms_num(syms)
        self.data: Optional['Scalar'] = None
        self.syms: List['Sym'] = list(syms)
        self.grad: 'Scalar' = cast_scalar(0)

    def _check_syms_num(self, syms):
        assert len(syms) == self.n_syms, \
            "invalid sym number: {}, ".format(len(syms)) + \
            "expected: {}".format(len(n_syms))

    def backward(self, grad: 'Scalar'=cast_scalar(1)) -> None:
        grad = cast_scalar(grad)
        self.grad += grad
        for i, sym in enumerate(self.syms):
            backward_grad = self.grad_fns[i](grad)
            sym.backward(backward_grad)


class Binary(Sym):
    n_syms = 2


class Add(Binary):
    grad_fns: List['GradFunc'] = [
        lambda grad: grad,
        lambda grad: grad,
    ]


class Multiply(Binary):
    grad_fns: List['GradFunc'] = [
    ]

from typing import List, Callable, Optional, Union
from functools import wraps

import numpy as np

Float = Union['int', 'float', 'np.float32', 'np.float64']
GradFunc = Callable[['np.float64'], 'np.float64']

def cast_float(scalar: 'Float') -> 'np.float64':
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)

def register_op(num_deps: int):
    def wrapper_cls(cls):
        def check_num_deps(func):
            def impl(self, *deps: 'Op') -> None:
                assert len(deps) == num_deps, \
                    "invalid deps number: {}, ".format(len(deps)) + \
                    "expected: {}".format(num_deps)
                func(self, *deps)
            return impl

        setattr(cls, "__init__", check_num_deps(getattr(cls, "__init__")))
        setattr(cls, "op_name", cls.__name__.lower())
        return cls
    return wrapper_cls


class Op(object):
    _grad_fns: List['GradFunc'] = []
    op_name: str
    data: Optional['Float'] = None
    grad: 'Float' = cast_float(0)

    def __init__(self, *deps: 'Op')-> None:
        self.deps: List['Op'] = list(deps)

    def backward(self, grad: 'Float'=cast_float(1)) -> None:
        grad = cast_float(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

    def forward(self) -> None:
        raise NotImplementedError

    def autograph_backward(self) -> 'Op':
        raise NotImplementedError

    def autograph_forward(self) -> 'Op':
        raise NotImplementedError

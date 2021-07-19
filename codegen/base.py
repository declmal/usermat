from typing import Set, List, Dict, Callable, Optional, Union
from functools import wraps

import numpy as np

Scalar = Union['int', 'float', 'np.float64']
GradFunc = Callable[['np.float64'], 'np.float64']

def cast_scalar(scalar: 'Scalar') -> 'np.float64':
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)


class N(object):
    _func_name: str = ""
    _op_name: str = ""
    _op_nm: Dict[str, Set[str]] = {}
    _name_manager: Dict[str, int] = {}

    @staticmethod
    def n(name: str="") -> str:
        if name == "":
            full_name = "{};{}".format(N._func_name, N._op_name)
        else:
            full_name = "{};{};{}".format(N._func_name, N._op_name, name)
        if full_name not in N._name_manager:
            cnt = N._name_manager[full_name] = 1
        else:
            N._name_manager[full_name] += 1
            cnt = N._name_manager[full_name]
        full_name = "{};{}".format(full_name, cnt)
        return full_name

    @staticmethod
    def register_op(op_name: str):
        def _register_func(func_name: str):
            def _wrapper(func):
                @wraps(func)
                def impl(*args, **kwargs):
                    prev_op_name = N._op_name
                    N._op_name = op_name
                    prev_func_name = N._func_name
                    N._func_name = func_name
                    ret = func(*args, **kwargs)
                    N._func_name = prev_func_name
                    N._op_name = prev_op_name
                    return ret
                return impl
            return _wrapper

        def wrapper(cls):
            if op_name in N._op_nm:
                raise NameError(
                    "Op: {} has been registered".format(op_name))
            op_nm = N._name_manager[op_name] = set()
            for func_name in [
                "__init__", "autograph_forward", "autograph_backward"]:
                if func_name in op_nm:
                    raise NameError(
                        "Function: {} has been registered, Op: {}".format(
                            func_name, func_nm))
                op_nm.add(func_name)
                func = getattr(cls, func_name)
                setattr(cls, func_name, _register_func(func_name)(func))
            return cls
        return wrapper


class Op(object):
    _grad_fns: List['GradFunc'] = []

    def __init__(self, *deps: 'Op', **params: 'Scalar') -> None:
        num_deps = len(self._grad_fns)
        assert len(deps) == num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(num_deps)
        self.data: Optional['Scalar'] = None
        self.deps: List['Op'] = list(deps)
        self.grad: 'Scalar' = cast_scalar(0)

    def backward(self, grad: 'Scalar'=cast_scalar(1)) -> None:
        grad = cast_scalar(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

    def autograph_backward(self) -> 'Op':
        raise NotImplementedError

    def autograph_forward(self) -> 'Op':
        raise NotImplementedError

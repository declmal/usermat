from typing import List
import sys, inspect

from codegen.base import Op, GradFunc, register_op

_clsmembers_base = dict(
    inspect.getmembers(
        sys.modules[__name__], inspect.isclass))

@register_op(0)
class Scalar(Op):
    def autograph_backward(self) -> "Op":
        raise NotImplementedError


@register_op(0)
class Var(Op):
    def autograph_backward(self) -> "Op":
        pass


@register_op(2)
class Add(Op):
    _grad_fns: List['GradFunc'] = [
        lambda grad: grad,
        lambda grad: grad,
    ]

    def autograph_backward(self) -> 'Op':
        pass


@register_op(2)
class Multiply(Op):
    def autograph_backward(self) -> "Op":
        pass

available_ops = {
    name: cls \
    for name, cls in inspect.getmembers(
        sys.modules[__name__], inspect.isclass) \
    if name not in _clsmembers_base
}

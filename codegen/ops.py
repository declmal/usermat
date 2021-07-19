from typing import List

from codegen.base import Op, N, GradFunc

@N.register_op("var")
class Var(Op):
    pass


@N.register_op("add")
class Add(Op):
    _grad_fns: List['GradFunc'] = [
        lambda grad: grad,
        lambda grad: grad,
    ]

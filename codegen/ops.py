from typing import List

from codegen.base import Op, GradFuncType, register_op, EquivFuncType

var_equiv_func: "EquivFuncType" = lambda op_type, ops: []
swappable_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(
            op_type, ",".join([ops[0].op_type, ops[1].op_type])),
        "{}:[{}]".format(
            op_type, ",".join([ops[1].op_type, ops[0].op_type])),
    ]

@register_op(0)
class Scalar(Op):
    pass


@register_op(0, equiv_func=var_equiv_func)
class Var(Op):
    def autograph_backward(self) -> "Op":
        pass


@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]

    def autograph_backward(self) -> "Op":
        pass


@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    def autograph_backward(self) -> "Op":
        pass

from typing import List, Dict

import numpy as np

from codegen.base import Op, GradFuncType, \
    register_op, EquivFuncType, supported_ops

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
    def forward(self) -> None:
        pass


@register_op(0, equiv_func=var_equiv_func)
class Var(Op):
    def forward(self) -> None:
        pass


@register_op(1)
class Sin(Op):
    def forward(self) -> None:
        self.data = np.sin(self.deps[0].data)


@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]

    def forward(self) -> None:
        self.data = self.deps[0].data + self.deps[1].data

    def autograph_backward(self) -> "Op":
        pass


@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    def forward(self) -> None:
        self.data = self.deps[0].data * self.deps[1].data

    def autograph_backward(self) -> "Op":
        pass


@register_op(2)
class DtrMultiply(Op):
    pass


@register_op(2)
class DtrAdd(Op):
    pass

def register_op_def(cls):
    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            equivs: List[str] = op_cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op_id: int = OpDef.equiv_map[equiv]
                    equiv_op: "Op" = OpDef.id_map[equiv_op_id]
                    return equiv_op
            cid: int = OpDef.current_id
            for equiv in equivs:
                OpDef.equiv_map[equiv] = cid
            op: "Op" = op_cls(*deps)
            op.set_id(cid)
            OpDef.id_map[cid] = op
            OpDef.current_id = cid + 1
            return op
        return wrapper

    for op_cls in supported_ops.values():
        op_type = getattr(op_cls, "op_type")
        setattr(cls, op_type, op_func(op_cls))
    return cls


@register_op_def
class OpDef(object):
    current_id: int = 0
    equiv_map: Dict[str, int] = {}
    id_map: Dict[int, 'Op'] = {}

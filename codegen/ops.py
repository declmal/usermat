from typing import List, Dict, Optional

import numpy as np

from codegen.base import Op, GradFuncType, \
    register_op, EquivFuncType, supported_ops, \
    cast_float, Float

var_equiv_func: "EquivFuncType" = lambda op_type, ops: []
swappable_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: list(set([
        "{}:[{}]".format(
            op_type, ",".join([str(ops[0].id), str(ops[1].id)])),
        "{}:[{}]".format(
            op_type, ",".join([str(ops[1].id), str(ops[0].id)])),
    ]))


@register_op(0)
class Scalar(Op):
    def reset(self) -> None:
        self.diff.clear()
        self.sym = None


@register_op(0, equiv_func=var_equiv_func)
class Var(Op):
    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [None]*len(var_seq)
        self.diff[var_seq[self.id]] = OpDef.scalar(1.0)


@register_op(1)
class Negative(Op):
    def forward(self) -> None:
        self.data = -self.deps[0].data


@register_op(1)
class Sin(Op):
    def forward(self) -> None:
        self.data = np.sin(self.deps[0].data)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x = self.deps[0]
        y = OpDef.cos(x)
        self.diff.clear()
        for di in self.deps[0].diff:
            if di is None:
                op = None
            else:
                op = OpDef.multiply(y, di)
            self.diff.append(op)


@register_op(1)
class Cos(Op):
    def forward(self) -> None:
        self.data = np.cos(self.deps[0].data)


@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]

    def forward(self) -> None:
        self.data = self.deps[0].data + self.deps[1].data

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(var_seq)):
            if d0[i] is None:
                if d1[i] is None:
                    op = None
                else:
                    op = d1[i]
            else:
                if d1[i] is None:
                    op = d0[i]
                else:
                    op = OpDef.add(d0[i], d1[i])
            self.diff.append(op)


@register_op(2)
class PlusScalar(Op):
    def forward(self) -> None:
        self.data = self.deps[0].data + self.deps[1].data


@register_op(2)
class Subtract(Op):
    def forward(self) -> None:
        self.data = self.deps[0].data - self.deps[1].data


@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    def forward(self) -> None:
        self.data = self.deps[0].data * self.deps[1].data

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(d0)):
            if d0[i] is None:
                if d1[i] is None:
                    op = None
                else:
                    op = OpDef.multiply(x0, d1[i])
            else:
                op1 = OpDef.multiply(x1, d0[i])
                if d1[i] is None:
                    op = op1
                else:
                    op2 = OpDef.multiply(x0, d1[i])
                    op = OpDef.add(op1, op2)
            self.diff.append(op)


@register_op(2)
class Divide(Op):
    def forward(self) -> None:
        self.data = self.deps[0].data / self.deps[1].data

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(d0)):
            if d0[i] is None:
                if d1[i] is None:
                    op = None
                else:
                    op1 = OpDef.multiply(self, d1[i])
                    op2 = OpDef.divide(op1, x1)
                    op = OpDef.negative(op2)
            else:
                if d1[i] is None:
                    op = OpDef.divide(d0[i], x1)
                else:
                    op1 = OpDef.multiply(self, d1[i])
                    op2 = OpDef.subtract(d0[i], op1)
                    op = OpDef.divide(op2, x1)
            self.diff.append(op)

def register_op_def(cls):
    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            equivs: List[str] = op_cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op: "Op" = OpDef.equiv_map[equiv]
                    return equiv_op
            op: "Op" = op_cls(*deps)
            OpDef.set_id(op)
            for equiv in equivs:
                OpDef.equiv_map[equiv] = op
            return op
        return wrapper

    def scalar_func(op_cls):
        def wrapper(data: "Float") -> "Op":
            nv: "np.float64" = cast_float(data)
            if nv in OpDef.scalar_map:
                return OpDef.scalar_map[nv]
            op: "Op" = op_cls()
            OpDef.set_id(op)
            op.set_data(data)
            OpDef.scalar_map[nv] = op
            return op
        return wrapper

    for op_cls in supported_ops.values():
        op_type = getattr(op_cls, "op_type")
        if op_type == "scalar":
            setattr(cls, op_type, scalar_func(op_cls))
        else:
            setattr(cls, op_type, op_func(op_cls))
    return cls


@register_op_def
class OpDef(object):
    current_id: int = 0
    equiv_map: Dict[str, "Op"] = {}
    scalar_map: Dict["np.float64", "Op"] = {}

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.scalar_map.clear()

    @staticmethod
    def set_id(op: "Op") -> None:
        op.set_id(OpDef.current_id)
        OpDef.current_id += 1
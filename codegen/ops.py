from typing import List, Dict, Optional

import numpy as np
import mxnet as mx

from codegen.base import Op, GradFuncType, \
    register_op, EquivFuncType, supported_ops, \
    cast_float, Float, FwdFuncType, Zero, One

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
    def forward(self):
        pass

    def reset(self) -> None:
        self.diff.clear()
        self.sym = None

    def to_sym(self) -> None:
        name = "{},{},{}".format(self.id, self.op_type, self.data)
        self.sym = mx.sym.var(name=name)


@register_op(0, equiv_func=var_equiv_func)
class Var(Op):
    def forward(self):
        pass

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [None]*len(var_seq)
        self.diff[var_seq[self.id]] = OpDef.scalar(1.0)


@register_op(1)
class Negative(Op):
    fwd_func: FwdFuncType = lambda v: -v


@register_op(1)
class Sin(Op):
    fwd_func: FwdFuncType = lambda v: np.sin(v)

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
    fwd_func: FwdFuncType = lambda v: np.cos(v)


@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]
    fwd_func: FwdFuncType = lambda v0, v1: v0 + v1

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
    fwd_func: FwdFuncType = lambda v0, v1: v0 + v1


@register_op(2)
class Subtract(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 - v1


@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 * v1

    @classmethod
    def degenerate(
        cls, *deps: "Op") -> (type, List["Op"], Optional["Float"]):
        x0, x1 = deps
        if isinstance(x0, Scalar) and x0.data == Zero or \
            isinstance(x1, Scalar) and x1.data == Zero:
            return Scalar, [], Zero
        if isinstance(x0, Scalar) and isinstance(x1, Scalar):
            data = self.__class__.fwd_func(x0, x1)
            return Scalar, [], data
        if isinstance(x0, Scalar) and x0.data == One:
            return x1.__class__, x1.deps, None
        if isinstance(x1, Scalar) and x1.data == One:
            return x0.__class__, x0.deps, None
        return cls, deps, None

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
    fwd_func: FwdFuncType = lambda v0, v1: v0 / v1

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

    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            # check degenerality
            nop_cls, ndeps, data = op_cls.degenerate(*deps)
            # check scalar
            if data is not None:
                assert nop_cls == Scalar, \
                    "invalid op: {}".format(op.op_type)
                assert len(ndeps) == 0, \
                    "invalid number of deps: {}".format(len(ndeps))
                return scalar_func(Scalar)(data)
            # check availability
            equivs: List[str] = nop_cls.op_equiv_func(ndeps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op: "Op" = OpDef.equiv_map[equiv]
                    return equiv_op
            op: "Op" = nop_cls(*ndeps)
            OpDef.set_id(op)
            for equiv in equivs:
                OpDef.equiv_map[equiv] = op
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

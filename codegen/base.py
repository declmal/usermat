from typing import Dict, List, Callable, Optional, Union, Any
import logging

import numpy as np
import mxnet as mx


Float = Union["int", "float", "np.float32", "np.float64"]
GradFuncType = Callable[["np.float64"], "np.float64"]

def cast_float(scalar: "Float") -> "np.float64":
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)

supported_ops: Dict[str, "Op"] = {}
EquivFuncType = Callable[[str, List["Op"]], List[str]]
OpEquivFuncType = Callable[[List["Op"]], List[str]]
default_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(
            op_type, ",".join([str(op.op_id) for op in ops]))]

def register_op(
    num_deps: int,
    equiv_func: "EquivFuncType"=default_equiv_func):
    def wrapper(cls):
        op_type: str = cls.__name__.lower()
        assert op_type not in supported_ops, \
            "duplicate op_type: {}".format(op_type)
        setattr(cls, "op_type", op_type)
        setattr(cls, "num_deps", num_deps)

        def op_equiv_func(ops: List["Op"]) -> List[str]:
            assert len(ops) == num_deps, \
                "invalid number of ops: {}, expected: {}".format(
                    len(ops), num_deps)
            return equiv_func(op_type, ops)

        setattr(cls, "op_equiv_func", op_equiv_func)
        supported_ops[op_type] = cls
        return cls
    return wrapper


class Op(object):
    _grad_fns: List["GradFuncType"] = []
    op_type: Optional[str] = None
    num_deps: Optional[int] = None
    op_equiv_func: Optional["OpEquivFuncType"] = None

    def __init__(self, *deps: "Op")-> None:
        assert len(deps) == self.num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(self.num_deps)
        self.deps: List["Op"] = list(deps)
        self.data: "Float" = cast_float(0)
        self.grad: "Float" = cast_float(0)
        self.op_id: int = -1
        self.op_sym: Optional["mx.sym.Symbol"] = None

    def set_id(self, op_id: int) -> None:
        self.op_id = op_id

    def set_data(self, data: 'Float') -> None:
        self.data = data

    def backward(self, grad: "Float"=cast_float(1)) -> None:
        grad = cast_float(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

    def forward(self) -> None:
        pass

    def reset(self) -> None:
        self.data = cast_float(0)
        self.op_mx = None

    def info(self, logger=logging.getLogger("op_info")) -> None:
        logger.info("id: {}, data: {}".format(self.op_id, self.data))

    def to_mx(self) -> None:
        name = "{},{}".format(self.op_id, self.op_type)
        dep_syms = [dep.op_sym for dep in self.deps]
        if not dep_syms:
            self.op_sym = mx.sym.var(name=name)
        else:
            self.op_sym = mx.sym.add_n(*dep_syms, name=name)

    def autograph_backward(
        self, var_seq: Dict[int,int]) -> List[Optional["Op"]]:
        raise NotImplementedError

    def autograph_forward(self) -> "Op":
        raise NotImplementedError

from typing import Dict, List, Callable, Optional, Union, Any
import logging

import numpy as np
import mxnet as mx

Float = Union["int", "float", "np.float32", "np.float64"]
GradFuncType = Callable[["np.float64"], "np.float64"]
FwdFuncType = Callable[[List["np.float64"]], "np.float64"]
Zero = np.float64(0.0)
One = np.float64(1.0)

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
            op_type, ",".join([str(op.id) for op in ops]))]

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
    fwd_func: FwdFuncType
    is_scalar: bool = False

    def __init__(self, *deps: "Op")-> None:
        assert len(deps) == self.num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(self.num_deps)
        self.deps: List["Op"] = list(deps)
        self.data: "Float" = cast_float(0)
        self.grad: "Float" = cast_float(0)
        self.id: int = -1
        self.diff: List["Op"] = []
        self.sym: Optional["mx.sym.Symbol"] = None

    def forward(self) -> None:
        vs: List["np.float64"] = [dep.data for dep in self.deps]
        self.data = self.__class__.fwd_func(*vs)

    @classmethod
    def to_scalar(cls, *deps: "Op") -> Optional["Float"]:
        if deps and all([dep.is_scalar for dep in deps]):
            vs: List["np.float64"] = [dep.data for dep in deps]
            return self.__class__.fwd_func(*vs)
        return None

    @classmethod
    def degenerate(cls, *deps: "Op") -> Optional["Op"]:
        return None

    def set_id(self, op_id: int) -> None:
        self.id = op_id

    def set_data(self, data: 'Float') -> None:
        self.data = data

    def backward(self, grad: "Float"=cast_float(1)) -> None:
        grad = cast_float(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

    def reset(self) -> None:
        self.data = cast_float(0)
        self.diff.clear()
        self.sym = None

    def info(self) -> str:
        return "id: {}, op_type: {}, data: {}".format(
            self.id, self.op_type, self.data)

    def display(self, logger=logging.getLogger("op_info")) -> None:
        logger.info(self.info())

    def to_sym(self) -> None:
        name = "{},{}".format(self.id, self.op_type)
        dep_syms = [dep.sym for dep in self.deps]
        if not dep_syms:
            self.sym = mx.sym.var(name=name)
        else:
            self.sym = mx.sym.add_n(*dep_syms, name=name)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        raise NotImplementedError

    def autograph_forward(self) -> "Op":
        raise NotImplementedError

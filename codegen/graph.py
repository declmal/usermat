from typing import List, Set
import logging

from codegen.base import Float, Op, cast_float
import utils

def register_dfs(impl):
    def dfs(op: "Op", visited: Set[str])-> None:
        if op.op_id in visited:
            return
        visited.add(op.op_id)
        for dep in op.deps:
            dfs(dep, visited)
        impl(op)

    return dfs

@register_dfs
def op_forward(op: "Op") -> None:
    op.forward()

@register_dfs
def op_reset(op: "Op") -> None:
    op.reset()

@register_dfs
def op_info(op: "Op", logger=logging.getLogger("op_info")) -> None:
    logger.info(
        "id: {}, data: {}".format(op.op_id, op.data))


class Graph(object):
    def __init__(self, inps: "Op", outs: "Op") -> None:
        self.inps: List["Op"] = inps
        self.outs: List["Op"] = outs
        self.reset()

    def reset(self) -> None:
        for out in self.outs:
            op_reset(out, set())

    def set_input(self, *datas: "Float") -> None:
        assert len(datas) == len(self.inps), \
            "invalid number of datas: {}, expected: {}".format(
                len(datas), len(self.inps))
        for i, inp in enumerate(self.inps):
            inp.set_data(cast_float(datas[i]))

    def get_output(self) -> List["Float"]:
        for out in self.outs:
            op_forward(out, set())
        return [out.data for out in self.outs]

    def get_info(self) -> None:
        for out in self.outs:
            op_info(out, set())

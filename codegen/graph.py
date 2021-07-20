from typing import List, Set, Dict, Any, Optional
from os import path
import json

import mxnet as mx

from codegen.base import Float, Op, cast_float

def register_dfs(impl):
    def dfs(op: "Op", visited: Set[int])-> None:
        assert op.op_id != -1, "invalid op_id: {}".format(op.op_id)
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
def op_info(op: "Op") -> None:
    op.info()

@register_dfs
def op_to_mx(op: "Op") -> None:
    op.to_mx()


class Graph(object):
    def __init__(self, inps: List["Op"], outs: List["Op"]) -> None:
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

    def get_mx(self, json_path: str=path.expanduser("~/mx.json")) -> None:
        for out in self.outs:
            op_to_mx(out, set())
        sym_outs = [out.op_sym for out in self.outs]
        sym = mx.sym.Group(sym_outs)
        arr: Dict[str, Any] = json.loads(sym.tojson())
        nodes: List[Dict[str, Any]] = arr["nodes"]
        for node in nodes:
            op_type = node["op"]
            if op_type == "add_n":
                node["op"] = node["name"]
        with open(json_path, "w") as f:
            f.write(json.dumps(arr))

    def autograph_backward(self) -> "Graph":
        var_seq: Dict[int, int] = \
            {self.inps[i].op_id: i for i in range(len(self.inps))}
        outs = []
        for out in self.outs:
            for o in out.autograph_backward(var_seq):
                outs.append(o)
        return Graph(self.inps, outs)


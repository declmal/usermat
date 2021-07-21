from typing import List, Set, Dict, Any
from os import path
import json

import mxnet as mx

from codegen.base import Float, Op, cast_float

def register_dfs(impl):
    def dfs(op: "Op", visited: Set[int], **kwargs) -> None:
        assert op.id != -1, "invalid id: {}".format(op.id)
        if op.id in visited:
            return
        visited.add(op.id)
        for dep in op.deps:
            dfs(dep, visited, **kwargs)
        impl(op, **kwargs)
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
def op_to_sym(op: "Op") -> None:
    op.to_sym()

@register_dfs
def op_autograph_backward(op: "Op", **kwargs) -> None:
    op.autograph_backward(kwargs.get("var_seq"))


class Graph(object):
    def __init__(self, inps: List["Op"], outs: List["Op"]) -> None:
        self.inps: List["Op"] = inps
        self.outs: List["Op"] = outs
        self.reset()

    def reset(self) -> None:
        visited = set()
        for out in self.outs:
            op_reset(out, visited)

    def set_input(self, *datas: "Float") -> None:
        assert len(datas) == len(self.inps), \
            "invalid number of datas: {}, expected: {}".format(
                len(datas), len(self.inps))
        for i, inp in enumerate(self.inps):
            inp.set_data(cast_float(datas[i]))

    def get_output(self) -> List["Float"]:
        visited = set()
        for out in self.outs:
            op_forward(out, visited)
        return [out.data for out in self.outs]

    def get_info(self) -> None:
        visited = set()
        for out in self.outs:
            op_info(out, visited)

    def to_sym(self, json_path: str=path.expanduser("~/mx.json")) -> None:
        visited = set()
        for out in self.outs:
            op_to_sym(out, visited)
        sym_outs = [out.sym for out in self.outs]
        sym = mx.sym.Group(sym_outs)
        arr: Dict[str, Any] = json.loads(sym.tojson())
        nodes: List[Dict[str, Any]] = arr["nodes"]
        for node in nodes:
            op_type = node["op"]
            if op_type == "add_n":
                node["op"] = node["name"]
        with open(json_path, "w") as f:
            f.write(json.dumps(arr, indent=4))

    def autograph_backward(self) -> "Graph":
        var_seq: Dict[int, int] = \
            {self.inps[i].id: i for i in range(len(self.inps))}
        visited = set()
        for out in self.outs:
            op_autograph_backward(out, visited, var_seq=var_seq)
        outs = [o for o in out.diff for out in self.outs]
        return Graph(self.inps, outs)

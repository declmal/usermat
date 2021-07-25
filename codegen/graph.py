from typing import List, Set, Dict, Any, Optional
from os import path
import json

import mxnet as mx

from codegen.base import Float, Op, cast_float
from codegen.ops import OpDef as od
from codegen.sym_utils import sym_rename


def topo_sort(op_group: List["Op"]) -> List["Op"]:
    topo_seq: List["Op"] = []
    cur: List["Op"] = op_group
    visited: Dict[int, int] = {}
    while cur:
        cop: "Op" = cur[-1]
        flag = True
        for dep in cop.deps:
            nid = dep.id
            if nid not in visited:
                cur.append(dep)
                visited[nid] = 1
                flag = False
            elif visited[nid] == 1:
                flag = False
        if flag:
            topo_seq.append(cop)
            visited[cop.id] = 2
            cur.pop()
    return topo_seq

def topo_visit(
    ops: List["Op"], graph: Dict[int, str],
    call_back: str) -> List["Op"]:
    op_ids = [op.id for op in ops]
    graph = {}
    for op in topo_sort(ops):
        op_id = op.id
        ndeps = []
        for dep in op.deps:
            assert dep.id in graph, \
                "dep_id: {}, op_id: {}".format(dep_id, op_id)
            ndeps.append(dep)
        opt_func = getattr(op, call_back)
        nop = opt_func(*ndeps, op_id=op_id)
        graph[op_id] = nop
    nops = [graph[op_id] for op_id in op_ids]
    return nops

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
def op_display(op: "Op") -> None:
    op.display()

@register_dfs
def op_to_sym(op: "Op") -> None:
    op.to_sym()

@register_dfs
def op_autograph_backward(op: "Op", **kwargs) -> None:
    op.autograph_backward(kwargs.get("var_seq"))


class Graph(object):
    def __init__(
        self, inps: List["Op"], outs: List["Op"],
        out_appends: Optional[List[str]]=None) -> None:
        self.inps: List["Op"] = inps
        self.outs: List["Op"] = outs
        self.out_appends: Optional[List[str]] = out_appends \
            if out_appends is not None else \
            ["Out:{}".format(out.id) for out in outs]
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

    def forward(self) -> List["Float"]:
        visited = set()
        for out in self.outs:
            op_forward(out, visited)
        return [out.data for out in self.outs]

    def display(self) -> None:
        visited = set()
        for out in self.outs:
            op_display(out, visited)

    def to_sym(self, json_path: str=path.expanduser("~/mx.json")) -> None:
        visited = set()
        for out in self.outs:
            op_to_sym(out, visited)
        sym_outs = []
        for i, out in enumerate(self.outs):
            assert out is not None and out.sym is not None
            name = "{}##{}".format(
                out.sym.attr("name"), self.out_appends[i])
            sym = sym_rename(out.sym, name)
            sym_outs.append(sym)
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
        out_appends: List[str] = []
        for out in self.outs:
            op_autograph_backward(out, visited, var_seq=var_seq)
            for i, o in enumerate(out.diff):
                assert o is not None, \
                    "invalid diff: {}, op: {}".format(o.info, out.info)
                name = "Diff:{},{}".format(out.id, self.inps[i].id)
                out_appends.append(name)
        outs = [o for o in out.diff for out in self.outs]
        return Graph(self.inps, outs, out_appends=out_appends)

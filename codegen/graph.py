from typing import List, Set, Dict, Any, Optional
from os import path
import json

import mxnet as mx

from codegen.ops import OpDef as od, Float, Op, \
    cast_float, Scalar, Var, get_opt
from codegen.sym_utils import sym_rename

def topo_sort(op_group: List["Op"]) -> List["Op"]:
    topo_seq = []
    visited = {}
    for root in op_group:
        rid = root.id
        if rid in visited:
            assert visited[rid] == 2
            continue
        cur = [root]
        visited[rid] = 1
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
    inps: List["Op"], outs: List["Op"], callback: str) -> List["Op"]:
    inp_ids = [op.id for op in inps]
    out_ids = [op.id for op in outs]
    graph = {}
    od.reset(clear_scalar=False)
    for op in topo_sort(outs):
        op_id = op.id
        if isinstance(op, Scalar):
            data = op.data
            nop = od.scalar(data)
            graph[op_id] = nop
        else:
            ndeps = []
            for dep in op.deps:
                dep_id = dep.id
                assert dep_id in graph, \
                    "dep_id: {}, op_id: {}".format(dep_id, op_id)
                ndep = graph[dep_id]
                ndeps.append(ndep)
            opt_func = get_opt(op, callback)
            nop = opt_func(*ndeps)
            graph[op_id] = nop
    ninps = [graph[op_id] for op_id in inp_ids]
    nouts = [graph[op_id] for op_id in out_ids]
    return ninps, nouts

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

def register_graph_opt(cls):
    def graph_opt(callback):
        def wrapper(self):
            self.inps, self.outs = \
                topo_visit(self.inps, self.outs, callback)
        return wrapper

    for callback in dir(Op):
        if callback.startswith("opt_"):
            setattr(cls, callback[4:], graph_opt(callback))
    return cls


@register_graph_opt
class Graph(object):
    def __init__(
        self, inps: List["Op"], outs: List["Op"],
        out_appends: Optional[List[str]]=None) -> None:
        self.inps: List["Op"] = inps
        self.outs: List["Op"] = outs
        ids = {op.id for op in topo_sort(self.outs)}
        for inp in self.inps:
            inp_id = inp.id
            assert inp_id in ids, \
                "invalid inp_id: {}, ids: {}".format(inp_id, ids)
        self.out_appends: Optional[List[str]] = out_appends \
            if out_appends is not None else \
            ["Out:{}".format(i) for i in range(len(self.outs))]
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
        for i, out in enumerate(self.outs):
            op_autograph_backward(out, visited, var_seq=var_seq)
            for j, o in enumerate(out.diff):
                assert o is not None, \
                    "invalid diff: {}, op: {}".format(o.info, out.info)
                name = "Diff:{},{}".format(i, j)
                out_appends.append(name)
        outs = [o for o in out.diff for out in self.outs]
        return Graph(self.inps, outs, out_appends=out_appends)

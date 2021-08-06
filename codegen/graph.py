from typing import List, Set, Dict, Any, Optional
from os import path
import json

import mxnet as mx

from codegen.ops import OpDef as od, Float, Op, \
    cast_float, Scalar, Var, get_opt
from codegen.sym_utils import sym_rename

def topo_sort(op_group: List["Op"]) -> List["Op"]:
    visited: Set[int] = set()
    sops: List["Op"] = []
    nxt: Dict[int,Set[int]] = {}
    for op in op_group:
        op_id = op.id
        nxt[op_id] = set()
        visited.add(op_id)
        sops.append(op)
    res: Dict[int,int] = {}
    op_map: Dict[int,"Op"] = {}
    zero_deps: List[int] = []
    while sops:
        cop = sops.pop()
        cid = cop.id
        assert cid not in op_map, cid
        op_map[cid] = cop
        num_deps = len({o.id for o in cop.deps})
        if num_deps == 0:
            zero_deps.append(cid)
        else:
            res[cid] = num_deps
        for dep in cop.deps:
            dep_id = dep.id
            if dep_id not in nxt:
                nxt[dep_id] = set()
            nxt[dep_id].add(cid)
            if dep_id in visited:
                continue
            visited.add(dep_id)
            sops.append(dep)
    topo_seq: List["Op"] = []
    while zero_deps:
        cid = zero_deps.pop()
        cop = op_map[cid]
        topo_seq.append(cop)
        for nid in nxt[cid]:
            assert nid in res and res[nid] > 0
            res[nid] -= 1
            if res[nid] == 0:
                zero_deps.append(nid)
                del res[nid]
    return topo_seq

def topo_visit(
    inps: List["Op"], outs: List["Op"], assert_ops: List["Op"],
    callback: str) -> List["Op"]:
    inp_ids = [op.id for op in inps]
    out_ids = [op.id for op in outs]
    assert_op_ids = [op.id for op in assert_ops]
    graph = {}
    od.reset() # reset the graph define here
    for op in topo_sort(outs + assert_ops):
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
            topo_func = get_opt(op, callback)
            nop = topo_func(*ndeps)
            graph[op_id] = nop
    ninps = [graph[op_id] for op_id in inp_ids]
    nouts = [graph[op_id] for op_id in out_ids]
    nassert_ops = [graph[op_id] for op_id in assert_op_ids]
    return ninps, nouts, nassert_ops

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
def op_infer_sign(op: "Op") -> None:
    op.infer_sign()

@register_dfs
def op_autograph_backward(op: "Op", **kwargs) -> None:
    op.autograph_backward(kwargs.get("var_seq"))

def register_graph_opt(cls):
    def graph_topo(callback):
        def wrapper(self):
            self.inps, self.outs, self.assert_ops = \
                topo_visit(self.inps, self.outs, self.assert_ops, callback)
            self.assert_op_ids = {op.id for op in self.assert_ops}
            assert len(self.assert_ops) == len(self.assert_op_ids)
            self.graph_assertion()
        return wrapper

    for callback in dir(Op):
        if callback.startswith("topo_"):
            setattr(cls, callback[5:], graph_topo(callback))
    return cls


@register_graph_opt
class Graph(object):
    def __init__(
        self, inps: List["Op"], outs: List["Op"], assert_ops: List["Op"]=[],
        out_appends: Optional[List[str]]=None) -> None:
        # validate and set inps
        graph_op_ids = {op.id for op in topo_sort(outs)}
        inp_ids = set()
        for inp in inps:
            inp_id = inp.id
            assert inp_id in graph_op_ids, \
                "inp_id: {} not found in graph_op_ids: {}".format(
                    inp_id, graph_op_ids)
            assert inp_id not in inp_ids, \
                "duplicate ops, inp_id: {}".format(inp_id)
        self.inps: List["Op"] = inps
        # validate and set outs
        out_ids = set()
        for out in outs:
            out_id = out.id
            assert out_id not in out_ids, \
                "duplicate ops, out_id: {}".format(out_id)
        self.outs: List["Op"] = outs
        # validate assert_ops
        self.assert_op_ids = set()
        for op in assert_ops:
            op_id = op.id
            assert op_id not in self.assert_op_ids, \
                "duplicate ops, op_id: {}".format(op_id)
            self.assert_op_ids.add(op_id)
        self.assert_ops: List["Op"] = assert_ops
        # no overlapping between out_ids and assert_op_ids
        total_out_ids = self.assert_op_ids.union(out_ids)
        assert len(total_out_ids) == len(self.assert_op_ids) + len(out_ids)
        # out_appends: suffix that comes after "##" of the output symbol name
        self.out_appends: Optional[List[str]] = out_appends \
            if out_appends is not None else \
            ["Out:{}".format(i) for i in range(len(self.outs))]
        self.reset()

    def get_inp_size(self):
        return len(self.inps)

    def set_input(self, *datas: "Float") -> None:
        assert len(datas) == len(self.inps), \
            "invalid number of datas: {}, expected: {}".format(
                len(datas), len(self.inps))
        for i, inp in enumerate(self.inps):
            inp.set_data(cast_float(datas[i]))

    def reset(self) -> None:
        visited = set()
        for assert_op in self.assert_ops:
            op_reset(assert_op, visited)
        for out in self.outs:
            op_reset(out, visited)

    def infer_sign(self) -> None:
        visited = set()
        for assert_op in self.assert_ops:
            op_infer_sign(assert_op, visited)
        for out in self.outs:
            op_infer_sign(out, visited)

    def forward(self) -> List["Float"]:
        visited = set()
        for assert_op in self.assert_ops:
            op_forward(assert_op, visited)
        for out in self.outs:
            op_forward(out, visited)
        return [out.data for out in self.outs]

    def display(self) -> None:
        visited = set()
        for assert_op in self.assert_ops:
            op_display(assert_op, visited)
        for out in self.outs:
            op_display(out, visited)

    def to_sym(self, json_path: str=path.expanduser("~/mx.json")) -> None:
        visited = set()
        for out in self.outs:
            op_to_sym(out, visited)
        for assert_op in self.assert_ops:
            op_to_sym(assert_op, visited)
        sym_outs = []
        for assert_op in self.assert_ops:
            assert assert_op is not None and out.sym is not None
            sym = assert_op.sym
            sym_outs.append(sym)
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
        return Graph(
            self.inps, outs, out_appends=out_appends,
            assert_ops=self.assert_ops)

    def pre_optimize(self) -> None:
        self.standardize()
        self.toscalar()
        self.degenerate()

    def graph_assertion(self) -> None:
        assert_ops = od.get_assert_ops()
        for op in assert_ops:
            op_id = op.id
            if op_id not in self.assert_op_ids:
                self.assert_ops.append(op)
                self.assert_op_ids.add(op_id)

    def post_optimize(self) -> None:
        pass

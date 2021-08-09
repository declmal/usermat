from os import path
import json

import mxnet as mx

from codegen.op_utils import cast_float
from codegen.op_def import Op, OpDef as od
from codegen.sym_utils import sym_rename

""" visit passes
"""
def topo_sort(op_group):
    visited = set()
    sops = []
    nxt = {}
    for op in op_group:
        op_id = op.id
        nxt[op_id] = set()
        visited.add(op_id)
        sops.append(op)
    res = {}
    op_map = {}
    zero_deps = []
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
    topo_seq = []
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

def topo_visit(inps, outs, callback, reverse=False):
    inp_ids = [op.id for op in inps]
    out_ids = [op.id for op in outs]
    graph = {}
    od.reset() # reset the graph define here
    for op in topo_sort(outs):
        op_id = op.id
        if isinstance(op, od.get_op_cls("scalar")):
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
            topo_func = od.get_opt(op, callback)
            nop = topo_func(*ndeps)
            graph[op_id] = nop
    ninps = [graph[op_id] for op_id in inp_ids]
    nouts = [graph[op_id] for op_id in out_ids]
    return ninps, nouts

def dfs_visit(op, visited, callback, **kwargs):
    assert op.id != -1, "invalid id: {}".format(op.id)
    if op.id in visited:
        return
    visited.add(op.id)
    for dep in op.deps:
        dfs_visit(dep, visited, callback, **kwargs)
    dfs_func = od.get_opt(op, callback)
    dfs_func(op, **kwargs)

""" graph
"""
def register_graph_opt(cls):
    def graph_topo(callback):
        def wrapper(self):
            self.inps, self.outs = topo_visit(
                self.inps, self.outs, callback)
        return wrapper

    for callback in dir(Op):
        if callback.startswith("topo_"):
            setattr(cls, callback[5:], graph_topo(callback))
    return cls


@register_graph_opt
class Graph(object):
    def __init__(self, inps, outs, out_appends=None):
        # validate and set inps
        inp_ids = set()
        for inp in inps:
            inp_id = inp.id
            assert inp_id not in inp_ids, \
                "duplicate ops, inp_id: {}".format(inp_id)
            inp_ids.add(inp_id)
        self.inps = inps
        # set outs
        self.outs = outs
        # out_appends: suffix that comes
        # after "##" of the output symbol name
        self.out_appends = out_appends \
            if out_appends is not None else \
            ["Out:{}".format(i) for i in range(len(self.outs))]
        self.reset()

    def get_inp_size(self):
        return len(self.inps)

    def set_input(self, *datas):
        assert len(datas) == len(self.inps), \
            "invalid number of datas: {}, expected: {}".format(
                len(datas), len(self.inps))
        for i, inp in enumerate(self.inps):
            inp.set_data(cast_float(datas[i]))

    def reset(self):
        visited = set()
        for out in self.outs:
            dfs_visit(out, visited, "dfs_reset")

    def forward(self):
        visited = set()
        for out in self.outs:
            dfs_visit(out, visited, "dfs_forward")
        return [out.data for out in self.outs]

    def display(self):
        visited = set()
        for out in self.outs:
            dfs_visit(out, visited, "dfs_display")

    def tosym(self, json_path=path.expanduser("~/mx.json")):
        visited = set()
        for out in self.outs:
            dfs_visit(out, visited, "dfs_tosym")
        sym_outs = []
        for i, out in enumerate(self.outs):
            assert out is not None and out.sym is not None
            name = "{}##{}".format(
                out.sym.attr("name"), self.out_appends[i])
            sym = sym_rename(out.sym, name)
            sym_outs.append(sym)
        sym = mx.sym.Group(sym_outs)
        arr = json.loads(sym.tojson())
        nodes = arr["nodes"]
        for node in nodes:
            op_type = node["op"]
            if op_type == "add_n":
                node["op"] = node["name"]
        with open(json_path, "w") as f:
            f.write(json.dumps(arr, indent=4))

    def autograph_backward(self):
        var_seq = {self.inps[i].id: i for i in range(len(self.inps))}
        visited = set()
        out_appends = []
        for i, out in enumerate(self.outs):
            dfs_visit(
                out, visited, "dfs_autograph_backward", var_seq=var_seq)
            # op_autograph_backward(out, visited, var_seq=var_seq)
            for j, o in enumerate(out.diff):
                assert o is not None, \
                    "invalid diff: {}, op: {}".format(o.info, out.info)
                name = "Diff:{},{}".format(i, j)
                out_appends.append(name)
        outs = [o for o in out.diff for out in self.outs]
        return Graph(self.inps, outs, out_appends=out_appends)

    def optimize(self):
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # polynomial,monomial
        # add,power,multiply
        # divide,negative,subtract
        self.standardize()
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # polynomial,monomial
        # add,power,multiply
        self.degenerate()
        self.fuse()
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # polynomial,monomial

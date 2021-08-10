from os import path
import json

import mxnet as mx

from codegen.op_utils import cast_float
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op
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

def topo_visit(inps, outs, callback):
    inp_ids = [op.id for op in inps]
    out_ids = [op.id for op in outs]
    graph = {}
    # reset the graph define here since graph structure changes
    od.reset()
    for op in topo_sort(outs):
        op_id = op.id
        if isinstance(op, org.get_op_cls("scalar")):
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
            topo_func = org.get_opt(op, callback)
            nop = topo_func(*ndeps)
            graph[op_id] = nop
    ninps = [graph[op_id] for op_id in inp_ids]
    nouts = [graph[op_id] for op_id in out_ids]
    return ninps, nouts

def revtopo_visit(outs, callback):
    topo_seq = topo_sort(outs)
    topo_seq.reverse()
    for op in topo_seq:
        revtopo_func = org.get_opt(op, callback)
        revtopo_func(op)

def dfs(op, visited, callback, **kwargs):
    op_id = op.id
    assert op_id != -1, "invalid id: {}".format(op.id)
    if op_id in visited:
        return
    visited.add(op_id)
    for dep in op.deps:
        dfs(dep, visited, callback, **kwargs)
    dfs_func = org.get_opt(op, callback)
    dfs_func(op, **kwargs)

def dfs_visit(outs, callback, init_val_dict={}, **kwargs):
    visited = set()
    val_dict = init_val_dict.copy()
    for out in outs:
        dfs(out, visited, callback, val_dict=val_dict, **kwargs)
    return val_dict

""" graph
"""
def register_graph_topo(cls):
    def graph_topo(callback):
        def wrapper(self):
            self.inps, self.outs = topo_visit(
                self.inps, self.outs, callback)
        return wrapper

    for callback in dir(Op):
        if callback.startswith("topo_"):
            setattr(cls, callback[5:], graph_topo(callback))
    return cls


@register_graph_topo
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

    def propagate_assertion(self):
        revtopo_visit(self.outs, "revtopo_propagate_assertion")

    def forward(self, *datas):
        assert len(datas) == len(self.inps), \
            "invalid number of datas: {}, expected: {}".format(
                len(datas), len(self.inps))
        val_dict = {}
        for i, inp in enumerate(self.inps):
            inp_id = inp.id
            inp_v = datas[i]
            val_dict[inp_id] = inp_v
        val_dict = dfs_visit(
            self.outs, "dfs_forward", init_val_dict=val_dict)
        ret = []
        for out in self.outs:
            out_id = out.id
            out_v = val_dict[out_id]
            ret.append(out_v)
        return ret

    def display(self):
        dfs_visit(self.outs, "dfs_display")

    def tosym(self, json_path=path.expanduser("~/mx.json")):
        sym_dict = dfs_visit(self.outs, "dfs_tosym")
        sym_outs = []
        for i, out in enumerate(self.outs):
            out_id = out.id
            out_sym = sym_dict[out_id]
            name = "{}##{}".format(
                out_sym.attr("name"), self.out_appends[i])
            sym = sym_rename(out_sym, name)
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
        var_seq = {}
        for i in range(len(self.inps)):
            inp = self.inps[i]
            inp_id = inp.id
            var_seq[inp_id] = i
        out_appends = []
        diff_dict = dfs_visit(
            self.outs, "dfs_autograph_backward", var_seq=var_seq)
        for i, out in enumerate(self.outs):
            out_id = out.id
            out_diff = diff_dict[out_id]
            for j, o in enumerate(out_diff):
                assert o is not None, \
                    "invalid diff: {}, op: {}".format(o.info, out.info)
                name = "Diff:{},{}".format(i, j)
                out_appends.append(name)
        outs = []
        for out in self.outs:
            out_id = out.id
            out_diff = diff_dict[out_id]
            for o in out_diff:
                outs.append(o)
        dg = Graph(self.inps, outs, out_appends=out_appends)
        return dg

    def optimize(self):
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # add,power,multiply
        # divide,negative,subtract
        self.standardize()
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # add,power,multiply
        self.degenerate()
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # add,power,multiply
        self.infer_sign()

    def merge(self):
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # add,power,multiply
        self.fuse()
        # var,scalar
        # abs,sin,cos,lessthan,nomorethan
        # polynomial,monomial

    def infer_sign(self):
        sign_dict = dfs_visit(self.outs, "dfs_infer_sign")

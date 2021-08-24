from os import path
import json
import logging
import subprocess
from ctypes import byref, cdll, c_double

import mxnet as mx

from .utils.type_utils import cast_float, Zero
from .utils.sign_utils import infer_scalar_sign, OpSign, merge_sign
from .op_def import OpDef as od
from .op_reg import OpReg as org
from .base import Op
from .utils.sym_utils import sym_rename

""" visit passes
"""
def topo_sort(op_group):
    visited = set()
    sops = []
    nxt = {}
    for op in op_group:
        op_id = op.id
        if op_id not in visited:
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
    # adjacent asserts and deps
    assert_map = {}
    for ind, op in enumerate(topo_seq):
        op_type = op.op_type
        if op_type.startswith("assert"):
            op_id = op.id
            dep = op.deps[0]
            dep_id = dep.id
            assert dep_id not in assert_map, \
                "dep_id: {}, assert_id: {}".format(dep_id, op_id)
            assert_map[dep_id] = ind
    visited = set()
    ntopo_seq = []
    for op in topo_seq:
        op_id = op.id
        if op_id in visited:
            continue
        visited.add(op_id)
        ntopo_seq.append(op)
        if op_id in assert_map:
            assert_ind = assert_map[op_id]
            assert_op = topo_seq[assert_ind]
            assert_id = assert_op.id
            visited.add(assert_id)
            ntopo_seq.append(assert_op)
    return ntopo_seq

def graph_infer_sign(op):
    op_id = op.id
    if od.query_sign(op_id):
        return
    for dep in op.deps:
        graph_infer_sign(dep)
    od_sign_dict = od.get_sign_dict()
    infer_sign_func = org.get_opt(op, "dfs_infer_sign")
    infer_sign_func(op, od_sign_dict)

def topo_visit(
    inps, outs, asserts, callback, sign_dict_ref={},
    logger=logging.getLogger("topo_visit")):
    sign_dict = sign_dict_ref.copy()
    nsign_dict = {}
    inp_ids = [op.id for op in inps]
    out_ids = [op.id for op in outs]
    assert_ids = [op.id for op in asserts]
    ids = set(out_ids + assert_ids)
    topo_outs = [od.get_op(op_id) for op_id in ids]
    graph = {}
    # reset the graph define here since graph structure changes
    od.reset()
    topo_outs = asserts + outs
    for op in topo_sort(topo_outs):
        op_id = op.id
        if isinstance(op, org.get_op_cls("scalar")):
            data = op.data
            nop = od.scalar(data)
            sign = infer_scalar_sign(data)
        elif isinstance(op, org.get_op_cls("var")):
            name = op.name
            nop = od.var(name)
            sign = sign_dict[op_id]
        elif isinstance(op, org.get_op_cls("null")):
            nop = od.null()
            sign = OpSign.UNDEFINED
        else:
            ndeps = []
            for dep in op.deps:
                dep_id = dep.id
                assert dep_id in graph, \
                    "dep_id: {}, op_id: {}".format(dep_id, op_id)
                ndep = graph[dep_id]
                ndeps.append(ndep)
            topo_func = org.get_opt(op, callback)
            nop = topo_func(nsign_dict, *ndeps)
            if isinstance(nop, org.get_op_cls("scalar")):
                data = nop.data
                sign = infer_scalar_sign(data)
            elif isinstance(nop, org.get_op_cls("null")):
                sign = OpSign.UNDEFINED
            else:
                sign = sign_dict[op_id]
        # update graph
        graph[op_id] = nop
        # update sign
        graph_infer_sign(nop)
        nop_id = nop.id
        od_sign = od.get_sign(nop_id)
        merged_sign = merge_sign(od_sign, sign)
        # update nsign_dict
        nsign_dict[nop_id] = merged_sign
    ninps = []
    for i, op_id in enumerate(inp_ids):
        if op_id in graph:
            ninp = graph[op_id]
        else:
            logger.warning("the {}-th inp has been deprecated".format(i))
            ninp = od.null()
        ninps.append(ninp)
    nouts = [graph[op_id] for op_id in out_ids]
    nasserts = [graph[op_id] for op_id in assert_ids]
    return ninps, nouts, nasserts, nsign_dict

def revtopo_visit(outs, callback, sign_dict_ref={}):
    sign_dict = sign_dict_ref.copy()
    topo_seq = topo_sort(outs)
    topo_seq.reverse()
    for op in topo_seq:
        revtopo_func = org.get_opt(op, callback)
        revtopo_func(op, sign_dict)
    return sign_dict

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
        def wrapper(
            self, logger=logging.getLogger("graph.{}".format(callback))):
            sign_dict = self.infer_sign()
            self.inps, self.outs, self.asserts, nsign_dict = topo_visit(
                self.inps, self.outs, self.asserts, callback,
                sign_dict_ref=sign_dict)
            # validate input
            self.validate_inps()
            # update asserts
            self.fuse_asserts()
            logger.debug("graph has been optimized: {}".format(callback))
            return nsign_dict
        return wrapper

    for callback in dir(Op):
        if callback.startswith("topo_"):
            setattr(cls, callback[5:], graph_topo(callback))
    return cls


@register_graph_topo
class Graph(object):
    def __init__(
        self, inps, outs, asserts=None, out_appends=None):
        # validate and set inps
        self.inps = inps
        self.validate_inps()
        # set outs
        self.outs = outs
        # set asserts
        if asserts is None:
            self.asserts = []
        else:
            self.asserts = asserts
            self.fuse_asserts()
        # out_appends: suffix that comes
        # after "~~~~" of the output symbol name
        self.out_appends = out_appends \
            if out_appends is not None else \
            ["Out:{}".format(i) for i in range(len(self.outs))]
        # graph status
        self.status = 0
        self.code = None

    def validate_inps(self):
        inp_ids = set()
        var_ids = set()
        for inp in self.inps:
            inp_id = inp.id
            assert inp_id not in inp_ids, \
                "duplicate ops, inp_id: {}".format(inp_id)
            inp_ids.add(inp_id)
            if isinstance(
                inp, (org.get_op_cls("scalar"), org.get_op_cls("null"))):
                continue
            elif isinstance(inp, org.get_op_cls("var")):
                assert inp_id not in var_ids, \
                    "duplicate vars, inp_id: {}".format(inp_id)
                var_ids.add(inp_id)
            else:
                assert False, \
                    "unsupported op type: {} for inp".format(inp.op_type)

    def fuse_asserts(self):
        # fuse null
        nasserts = []
        for assert_op in self.asserts:
            if isinstance(assert_op, org.get_op_cls("null")):
                continue
            nasserts.append(assert_op)
        self.asserts = nasserts
        # fuse duplicate
        assert_ids = set()
        nasserts = []
        for assert_op in self.asserts:
            assert_id = assert_op.id
            if assert_id in assert_ids:
                continue
            assert_ids.add(assert_id)
            nasserts.append(assert_op)
        self.asserts = nasserts
        # merge asserts for the same op
        assert_map = {}
        for assert_op in self.asserts:
            dep = assert_op.deps[0]
            dep_id = dep.id
            if dep_id not in assert_map:
                assert_map[dep_id] = assert_op
            else:
                org_assert_op = assert_map[dep_id]
                nassert_op = assert_op.merge(org_assert_op)
                assert_map[dep_id] = nassert_op
        nasserts = []
        for assert_op in assert_map.values():
            nasserts.append(assert_op)
        self.asserts = nasserts
        # sort asserts
        self.asserts.sort()

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
        outs = self.asserts + self.outs
        val_dict = dfs_visit(
            outs, "dfs_forward", init_val_dict=val_dict)
        ret = []
        for out in self.outs:
            out_id = out.id
            out_v = val_dict[out_id]
            ret.append(out_v)
        return ret

    def display(self, logger=logging.getLogger("graph.display")):
        outs = self.asserts + self.outs
        dfs_visit(outs, "dfs_display", logger=logger)

    def info(self):
        outs = self.asserts + self.outs
        info_dict = dfs_visit(outs, "dfs_info")
        return info_dict

    def tosym(self, json_path=path.expanduser("~/mx.json")):
        outs = self.outs + self.asserts
        sym_dict = dfs_visit(outs, "dfs_tosym")
        sym_outs = []
        for i, out in enumerate(self.outs):
            out_id = out.id
            out_sym = sym_dict[out_id]
            name = "{}~~~~{}".format(
                out_sym.attr("name"), self.out_appends[i])
            sym = sym_rename(out_sym, name)
            sym_outs.append(sym)
        for i, out in enumerate(self.asserts):
            out_id = out.id
            out_sym = sym_dict[out_id]
            name = "{}~~~~Assert:{}".format(out_sym.attr("name"), i)
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

    def autodiff(self):
        assert self.status == 1, \
            "graph should be unified but not optimized"
        var_seq = {}
        for i in range(len(self.inps)):
            inp = self.inps[i]
            inp_id = inp.id
            var_seq[inp_id] = i
        out_appends = []
        diff_dict = dfs_visit(
            self.outs, "dfs_autodiff", var_seq=var_seq)
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
        self.outs = outs
        self.out_appends = out_appends
        self.degenerate()

    def sort_deps(self):
        outs = self.asserts + self.outs
        dfs_visit(outs, "dfs_sort_deps")

    def infer_sign(self, logger=logging.getLogger("graph.infer_sign")):
        sign_dict_1 = {}
        cnt = 0
        outs = self.outs + self.asserts
        while True:
            sign_dict_2 = dfs_visit(
                outs, "dfs_infer_sign", init_val_dict=sign_dict_1)
            flag1 = sign_dict_1 == sign_dict_2
            sign_dict_3 = revtopo_visit(
                outs, "revtopo_infer_sign", sign_dict_ref=sign_dict_2)
            flag2 = sign_dict_2 == sign_dict_3
            cnt += 1
            if flag1 and flag2:
                break
            sign_dict_1 = sign_dict_3
        logger.debug(
            "graph infer_sign has been run for {} passes".format(cnt))
        return sign_dict_1

    def unify(self, logger=logging.getLogger("graph.unify")):
        assert self.status == 0, "graph has already been unified"
        self.standardize()
        self.degenerate()
        logger.info("graph has been unified")
        self.status = 1

    def optimize(self, logger=logging.getLogger("graph.optimize")):
        assert self.status == 1, "graph has not been unified"
        # merge into mials
        info_dict_1 = {}
        cnt = 0
        while True:
            self.fuse()
            self.degenerate()
            info_dict_2 = self.info()
            cnt += 1
            if info_dict_1 == info_dict_2:
                break
            info_dict_1 = info_dict_2.copy()
        logger.debug(
            "graph merge has been run for {} passes".format(cnt))
        logger.info("graph has been merged")
        # insert asserts
        sign_dict_f = self.infer_sign()
        outs = self.outs + self.asserts
        sign_dict_0 = dfs_visit(outs, "dfs_infer_sign")
        assert len(sign_dict_f) == len(sign_dict_0)
        nasserts = []
        org_assert_ids = set()
        for assert_id in self.asserts:
            org_assert_ids.add(assert_id)
        for op_id, sign_f in sign_dict_f.items():
            assert op_id in sign_dict_f, op_id
            sign_0 = sign_dict_0[op_id]
            if sign_0 == sign_f:
                continue
            msign = merge_sign(sign_0, sign_f)
            assert msign == sign_f, \
                "sign_0: {}, sign_f: {}".format(sign_0, sign_f)
            zero = od.scalar(Zero)
            op = od.get_op(op_id)
            if sign_f == OpSign.NON_ZERO:
                assert_op = od.assertnonzero(op)
            elif sign_f == OpSign.ZERO:
                assert_op = od.assertzero(op)
            elif sign_f == OpSign.NON_NEGATIVE:
                assert_op = od.assertnonnegative(op)
            elif sign_f == OpSign.NON_POSITIVE:
                assert_op = od.assertnonpositive(op)
            elif sign_f == OpSign.POSITIVE:
                assert_op = od.assertpositive(op)
            elif sign_f == OpSign.NEGATIVE:
                assert_op = od.assertnegative(op)
            else:
                assert False
            assert_id = assert_op.id
            assert assert_id not in org_assert_ids, assert_id
            nasserts.append(assert_op)
        self.asserts += nasserts
        self.fuse_asserts()
        # validate asserts
        nsign_dict_f = self.infer_sign()
        for assert_op in self.asserts:
            assert_id = assert_op.id
            assert assert_id in nsign_dict_f
            del nsign_dict_f[assert_id]
        assert nsign_dict_f == sign_dict_f
        logger.info("graph assertions have been inserted")
        # zerify
        self.zerify()
        self.degenerate()
        logger.info("graph has been zerified")
        self.status = 2
        logger.info("graph has been optimized")

    def compile(self):
        assert self.status == 2, "graph has not been optimized"


    def __eq__(self, other):
        self.sort_deps()
        other.sort_deps()
        outs = self.outs + self.asserts
        outs1 = other.outs + other.asserts
        if len(outs) != len(outs1):
            return False
        for i in range(len(outs)):
            if outs[i] != outs1[i]:
                return False
        return True

import logging

import mxnet as mx
import numpy as np

from codegen.sign_utils import OpSign, merge_sign
from codegen.op_utils import Zero, cast_fraction, cast_float


""" base
"""
class Op(object):
    op_type = None
    op_equiv_func = None

    def __init__(self, *deps):
        self.deps = list(deps)
        self.id = -1
        self.assertions = []

    def insert_assertion(sign):
        self.assertions.append(sign)

    def set_id(self, op_id):
        self.id = op_id

    def info(self, with_data=True):
        deps_info = ""
        if self.deps:
            deps_info = "deps:" + \
                ",".join([str(dep.id) for dep in self.deps])
        data_info = ""
        if with_data:
            data_info = "data:{}".format(cast_float(self.data))
        s = "id:{},op_type:{}".format(self.id, self.op_type)
        _info = ",".join([s, data_info, deps_info])
        return _info

    @classmethod
    def default_op(cls, *deps):
        od_func = getattr(OpDef, cls.op_type)
        return od_func(*deps)

    @classmethod
    def topo_standardize(cls, *deps):
        return cls.default_op(*deps)

    @classmethod
    def topo_degenerate(cls, *deps):
        flag = True
        datas = []
        for dep in deps:
            if not isinstance(dep, OpDef.get_op_cls("scalar")):
                flag = False
                break
            data = dep.data
            datas.append(data)
        if not flag:
            return cls.default_op(*deps)
        cdata = cls.fwd_func(*datas)
        op = OpDef.scalar(cdata)
        return op

    @classmethod
    def topo_fuse(cls, *deps):
        return cls.default_op(*deps)

    def revtopo_propagate_assertion(self):
        pass

    def dfs_forward(self, val_dict):
        op_id = self.id
        assert op_id not in val_dict
        vs = []
        for dep in self.deps:
            dep_id = dep.id
            dep_v = val_dict[dep_id]
            vs.append(dep_v)
        v = self.__class__.fwd_func(*vs)
        val_dict[op_id] = v

    def dfs_display(
        self, logger=logging.getLogger("op_info"), with_data=True):
        _info = self.info(with_data=with_data)
        logger.debug(_info)

    def dfs_tosym(self, sym_dict):
        op_id = self.id
        assert op_id not in sym_dict
        name = self.info(with_data=False)
        dep_syms = []
        for dep in self.deps:
            dep_id = dep.id
            dep_sym = sym_dict[dep_id]
            dep_syms.append(dep_sym)
        if len(dep_syms) == 0:
            sym = mx.sym.var(name=name)
        else:
            sym = mx.sym.add_n(*dep_syms, name=name)
        sym_dict[op_id] = sym

    def dfs_infer_sign(self, sign_dict):
        cop_id = self.id
        if cop_id in sign_dict:
            osign = sign_dict[cop_id]
            sign = merge_sign(OpSign.INDEFINITE, osign)
        else:
            sign = OpSign.INDEFINITE
        sign_dict[cop_id] = sign

    def dfs_autograph_backward(self, diff_dict, var_seq):
        raise NotImplementedError


""" Op Manager
"""
class OpDef(object):
    """registration variables
    """
    supported_ops = {}
    op_supported_opts = {}
    supported_opts = { \
        k for k, v in Op.__dict__.items() \
        if k.startswith("topo_") or k.startswith("dfs_") or \
            k.startswith("revtopo_")}

    """status variables
    """
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    sign_map = {}

    """registration method
    """
    @staticmethod
    def _scalar_func(cls):
        def wrapper(data):
            nv = cast_fraction(data)
            if nv in OpDef.scalar_map:
                return OpDef.scalar_map[nv]
            op = cls(data)
            OpDef.set_id(op)
            op_id = op.id
            OpDef.scalar_map[nv] = op
            OpDef.id_map[op.id] = op
            return op
        return wrapper

    @staticmethod
    def _op_func(cls):
        def wrapper(*deps):
            equivs = cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op = OpDef.equiv_map[equiv]
                    return equiv_op
            op = cls(*deps)
            OpDef.set_id(op)
            for equiv in equivs:
                OpDef.equiv_map[equiv] = op
            op_id = op.id
            OpDef.id_map[op_id] = op
            op_type = getattr(cls, "op_type")
            return op
        return wrapper

    @staticmethod
    def register_op(
        valid_func=lambda *deps: None, equiv_func=lambda op_type, ops: []):
        def wrapper(cls):
            # set op type
            op_type = cls.__name__.lower()
            assert op_type not in OpDef.supported_ops, \
                "op_type: {} has been registered, ".format(op_type) + \
                "supported_ops: {}".format(OpDef.supported_ops)
            setattr(cls, "op_type", op_type)
            # set validate functions for init
            def op_valid_func(init_func):
                def _wrapper(self, *deps):
                    valid_func(*deps)
                    init_func(self, *deps)
                return _wrapper
            init_func = getattr(cls, "__init__")
            init_func = op_valid_func(init_func)
            setattr(cls, "__init__", init_func)
            # set equivalent function
            def op_equiv_func(ops):
                return equiv_func(op_type, ops)
            setattr(cls, "op_equiv_func", op_equiv_func)
            # update op manager
            OpDef.supported_ops[op_type] = cls
            _op_supported_opts = OpDef.op_supported_opts[op_type] = set()
            for k, v in cls.__dict__.items():
                if k.startswith("topo_") or k.startswith("dfs_") or \
                    k.startswith("revtopo_"):
                    _op_supported_opts.add(k)
            # set up op_def
            if op_type == "scalar":
                setattr(OpDef, op_type, OpDef._scalar_func(cls))
            else:
                setattr(OpDef, op_type, OpDef._op_func(cls))
            return cls
        return wrapper

    @staticmethod
    def get_opt(op, callback):
        op_type = op.op_type
        assert op_type in OpDef.supported_ops, \
            "Op: {} has not been registered".format(op_type)
        cls = OpDef.supported_ops[op_type]
        _op_supported_opts = OpDef.op_supported_opts[op_type]
        assert callback in _op_supported_opts, \
            "Op: {}, Opt: {} has not been registered".format(
                op_type, callback)
        opt_func = getattr(cls, callback)
        return opt_func

    @staticmethod
    def register_opt(callback):
        assert callback in OpDef.supported_opts, \
            "Opt: {} is not supported by Op".format(callback)
        def wrapper(cls):
            op_type = getattr(cls, "op_type")
            _op_supported_opts = OpDef.op_supported_opts[op_type]
            assert callback not in _op_supported_opts, \
                "Op: {}, Opt: {} has been registered".format(
                    op_type, callback)
            _op_supported_opts.add(callback)
            return cls
        return wrapper

    @staticmethod
    def get_op_cls(op_type):
        assert op_type in OpDef.supported_ops, \
            "please register Op: {} first".format(op_type)
        cls = OpDef.supported_ops[op_type]
        return cls

    """status method
    """
    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()
        OpDef.sign_map.clear()

    @staticmethod
    def get_sign(op_id):
        assert op_id in OpDef.sign_map, \
            "op id: {} not in sign_map: {}".format(op_id, OpDef.sign_map)
        return OpDef.sign_map[op_id]

    @staticmethod
    def set_id(op):
        op.set_id(OpDef.current_id)
        OpDef.current_id += 1

    @staticmethod
    def get_op(op_id):
        assert op_id in OpDef.id_map, "invalid op_id: {}".format(op_id)
        return OpDef.id_map[op_id]

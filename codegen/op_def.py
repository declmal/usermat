import logging

import mxnet as mx
import numpy as np

from codegen.infer_utils import OpSign
from codegen.op_utils import Zero, cast_fraction, cast_float


""" base
"""
class Op(object):
    _grad_fns = []
    op_type = None
    op_equiv_func = None
    # fwd_func
    is_scalar = False

    def __init__(self, *deps):
        self.deps = list(deps)
        self.data = cast_fraction(0)
        self.id = -1
        self.diff = []
        self.sym = None

    def set_id(self, op_id):
        self.id = op_id

    def set_data(self, data):
        self.data = data

    def get_sign(self):
        op_id = self.id
        sign = OpDef.get_sign(op_id)
        return sign

    def infer_sign(self):
        return OpSign.INDEFINITE

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
            if not isinstance(dep, OpDef.scalar_type()):
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

    def reset(self):
        self.data = cast_fraction(0)
        self.diff.clear()
        self.sym = None

    def forward(self):
        vs = [np.float64(dep.data) for dep in self.deps]
        self.data = self.__class__.fwd_func(*vs)

    def info(self, with_data=True):
        deps_info = ""
        if self.deps:
            deps_info = "deps:" + \
                ",".join([str(dep.id) for dep in self.deps])
        data_info = ""
        if with_data:
            data_info = "data:{}".format(cast_float(self.data))
        s = "id:{},op_type:{}".format(self.id, self.op_type)
        return ",".join([s, data_info, deps_info])

    def display(self, logger=logging.getLogger("op_info")):
        logger.debug(self.info())

    def to_sym(self):
        name = self.info(with_data=False)
        dep_syms = [dep.sym for dep in self.deps]
        if not dep_syms:
            self.sym = mx.sym.var(name=name)
        else:
            self.sym = mx.sym.add_n(*dep_syms, name=name)

    def autograph_backward(self, var_seq):
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
        if k.startswith("topo_") and type(v).__name__ == "classmethod"}

    """status variables
    """
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    assert_set = set()
    sign_map = {}

    """registration method
    """
    @staticmethod
    def _scalar_func(cls):
        def wrapper(data):
            nv = cast_fraction(data)
            if nv in OpDef.scalar_map:
                return OpDef.scalar_map[nv]
            op = cls()
            OpDef.set_id(op)
            op.set_data(nv)
            op_id = op.id
            OpDef.scalar_map[nv] = op
            OpDef.id_map[op.id] = op
            sign = op.infer_sign()
            OpDef.set_sign(op_id, sign)
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
            if op_type.startswith("assert"):
                OpDef.set_assert(op_id)
            sign = op.infer_sign()
            OpDef.set_sign(op_id, sign)
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
                if k.startswith("topo_") and \
                    type(v).__name__ == "classmethod":
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
    def scalar_type():
        op_type = "scalar"
        assert op_type in OpDef.supported_ops, \
            "please register Scalar Op first"
        cls = OpDef.supported_ops["scalar"]
        return cls

    """status method
    """
    @staticmethod
    def get_assert_ops():
        return list([OpDef.get_op(op_id) for op_id in OpDef.assert_set])

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()
        OpDef.assert_set.clear()
        OpDef.sign_map.clear()

    @staticmethod
    def set_sign(op_id, sign):
        assert op_id not in OpDef.sign_map, \
            "duplicate op id: {}, sign_map: {}".format(
                op_id, OpDef.sign_map)
        OpDef.sign_map[op_id] = sign

    @staticmethod
    def get_sign(op_id):
        assert op_id in OpDef.sign_map, \
            "op id: {} not in sign_map: {}".format(op_id, OpDef.sign_map)
        return OpDef.sign_map[op_id]

    @staticmethod
    def set_assert(op_id):
        assert op_id not in OpDef.assert_set, \
            "duplicate op id: {}, assert_set: {}".format(
                op_id, OpDef.assert_set)
        OpDef.assert_set.add(op_id)


    @staticmethod
    def set_id(op):
        op.set_id(OpDef.current_id)
        OpDef.current_id += 1

    @staticmethod
    def get_op(op_id):
        assert op_id in OpDef.id_map, "invalid op_id: {}".format(op_id)
        return OpDef.id_map[op_id]

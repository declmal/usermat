from .type_utils import cast_fraction
from .op_def import OpDef as od


""" Op Registration Manager
"""
class OpReg(object):
    supported_ops = {}
    ops_priority = {}
    op_supported_opts = {}
    supported_opts = set()

    @staticmethod
    def register_supported_opts(cls):
        for func_name in cls.__dict__.keys():
            if func_name.startswith("topo_") or \
                func_name.startswith("dfs_") or \
                func_name.startswith("revtopo_"):
                OpReg.supported_opts.add(func_name)
        return cls

    @staticmethod
    def _null_func(cls):
        def wrapper():
            if od.query_null():
                op = od.get_null()
                return op
            op = cls()
            od.set_null(op)
            od.set_op(op)
            return op
        return wrapper

    @staticmethod
    def _var_func(cls):
        def wrapper(name):
            if od.query_var(name):
                op = od.get_var(name)
                return op
            op = cls(name)
            od.set_var(name, op)
            od.set_op(op)
            return op
        return wrapper

    @staticmethod
    def _scalar_func(cls):
        def wrapper(data):
            nv = cast_fraction(data)
            if od.query_scalar(nv):
                op = od.get_scalar(nv)
                return op
            op = cls(nv)
            od.set_scalar(nv, op)
            od.set_op(op)
            return op
        return wrapper

    @staticmethod
    def _op_func(cls):
        def wrapper(*deps):
            equivs = cls.op_equiv_func(deps)
            for equiv in equivs:
                if od.query_equiv(equiv):
                    op = od.get_equiv(equiv)
                    return op
            op = cls(*deps)
            od.set_op(op)
            for equiv in equivs:
                od.set_equiv(equiv, op)
            return op
        return wrapper

    @staticmethod
    def register_op(
        valid_func=lambda *deps: None, equiv_func=lambda op_type, ops: []):
        def wrapper(cls):
            # set op type
            op_type = cls.__name__.lower()
            assert op_type not in OpReg.supported_ops, \
                "op_type: {} has been registered, ".format(op_type) + \
                "supported_ops: {}".format(OpReg.supported_ops)
            assert op_type not in OpReg.ops_priority
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
            OpReg.supported_ops[op_type] = cls
            OpReg.ops_priority[op_type] = len(OpReg.ops_priority)
            _op_supported_opts = OpReg.op_supported_opts[op_type] = set()
            for k, v in cls.__dict__.items():
                if k.startswith("topo_") or k.startswith("dfs_") or \
                    k.startswith("revtopo_"):
                    _op_supported_opts.add(k)
            # set up op_def
            if op_type == "scalar":
                setattr(od, op_type, OpReg._scalar_func(cls))
            elif op_type == "var":
                setattr(od, op_type, OpReg._var_func(cls))
            elif op_type == "null":
                setattr(od, op_type, OpReg._null_func(cls))
            else:
                setattr(od, op_type, OpReg._op_func(cls))
            return cls
        return wrapper

    @staticmethod
    def register_opt(callback):
        assert callback in OpReg.supported_opts, \
            "Opt: {} is not supported by Op".format(callback)
        def wrapper(cls):
            op_type = getattr(cls, "op_type")
            _op_supported_opts = OpReg.op_supported_opts[op_type]
            assert callback not in _op_supported_opts, \
                "Op: {}, Opt: {} has been registered".format(
                    op_type, callback)
            _op_supported_opts.add(callback)
            return cls
        return wrapper

    @staticmethod
    def get_op_cls(op_type):
        assert op_type in OpReg.supported_ops, \
            "please register Op: {} first".format(op_type)
        cls = OpReg.supported_ops[op_type]
        return cls

    @staticmethod
    def get_opt(op, callback):
        op_type = op.op_type
        assert op_type in OpReg.supported_ops, \
            "Op: {} has not been registered".format(op_type)
        cls = OpReg.supported_ops[op_type]
        _op_supported_opts = OpReg.op_supported_opts[op_type]
        assert callback in _op_supported_opts, \
            "Op: {}, Opt: {} has not been registered".format(
                op_type, callback)
        opt_func = getattr(cls, callback)
        return opt_func

    @staticmethod
    def get_priority(op_type):
        assert op_type in OpReg.ops_priority, \
            "invalid op_type: {}".format(op_type)
        priority = OpReg.ops_priority[op_type]
        return priority

from codegen.op_utils import cast_fraction


""" Op Registration and Definition Manager
"""
class OpDef(object):
    """registration variables
    """
    supported_ops = {}
    op_supported_opts = {}
    supported_opts = set()

    """definition variables
    """
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    sign_map = {}

    """registration method
    """
    @staticmethod
    def register_supported_opts(cls):
        for func_name in cls.__dict__.keys():
            if func_name.startswith("topo_") or \
                func_name.startswith("dfs_") or \
                func_name.startswith("revtopo_"):
                OpDef.supported_opts.add(func_name)
        return cls

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
    def get_op_cls(op_type):
        assert op_type in OpDef.supported_ops, \
            "please register Op: {} first".format(op_type)
        cls = OpDef.supported_ops[op_type]
        return cls

    """definition method
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

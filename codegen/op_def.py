from fractions import Fraction

from .type_utils import cast_fraction
from .sign_utils import OpSign


""" Op Definition Manager
"""
class OpDef(object):
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    var_map = {}
    null_op = None
    sign_dict = {}

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()
        OpDef.var_map.clear()
        OpDef.null_op = None
        OpDef.sign_dict.clear()

    @staticmethod
    def query_sign(op_id):
        assert isinstance(op_id, int), type(op_id)
        if op_id in OpDef.sign_dict:
            return True
        return False

    @staticmethod
    def get_sign(op_id):
        assert isinstance(op_id, int), type(op_id)
        ret = OpDef.query_sign(op_id)
        assert ret, "op_id: {} not in sign_dict".format(op_id)
        sign = OpDef.sign_dict[op_id]
        return sign

    @staticmethod
    def get_sign_dict():
        return OpDef.sign_dict

    @staticmethod
    def query_equiv(equiv):
        assert isinstance(equiv, str), type(equiv)
        if equiv in OpDef.equiv_map:
            return True
        return False

    @staticmethod
    def get_equiv(equiv):
        assert isinstance(equiv, str), type(equiv)
        ret = OpDef.query_equiv(equiv)
        assert ret, "equiv: {} not in equiv_map".format(equiv)
        op = OpDef.equiv_map[equiv]
        return op

    @staticmethod
    def set_equiv(equiv, op):
        assert isinstance(equiv, str), type(equiv)
        ret = OpDef.query_equiv(equiv)
        assert not ret, "equiv: {} already in equiv_map".format(equiv)
        OpDef.equiv_map[equiv] = op

    @staticmethod
    def query_op(op_id):
        assert isinstance(op_id, int) and op_id >= 0, op_id
        if op_id in OpDef.id_map:
            return True
        return False

    @staticmethod
    def get_op(op_id):
        assert isinstance(op_id, int) and op_id >= 0, op_id
        ret = OpDef.query_op(op_id)
        assert ret, "op_id: {} not in id_map".format(op_id)
        op = OpDef.id_map[op_id]
        return op

    @staticmethod
    def set_op(op):
        op_id = OpDef.current_id
        ret = OpDef.query_op(op_id)
        assert not ret, "op_id: {} already in id_map".format(op_id)
        op.set_id(op_id)
        OpDef.current_id += 1
        OpDef.id_map[op_id] = op

    @staticmethod
    def query_scalar(data):
        assert isinstance(data, Fraction), type(data)
        if data in OpDef.scalar_map:
            return True
        return False

    @staticmethod
    def get_scalar(data):
        assert isinstance(data, Fraction), type(data)
        ret = OpDef.query_scalar(data)
        assert ret, "data: {} not in scalar_map".format(data)
        scalar = OpDef.scalar_map[data]
        return scalar

    @staticmethod
    def set_scalar(data, scalar):
        assert isinstance(data, Fraction), type(data)
        ret = OpDef.query_scalar(data)
        assert not ret, "data: {} already in scalar_map".format(data)
        OpDef.scalar_map[data] = scalar

    @staticmethod
    def query_var(name):
        assert isinstance(name, str), type(name)
        if name in OpDef.var_map:
            return True
        return False

    @staticmethod
    def get_var(name):
        assert isinstance(name, str), type(name)
        ret = OpDef.query_var(name)
        assert ret, "name: {} not in var_map".format(name)
        var = OpDef.var_map[name]
        return var

    @staticmethod
    def set_var(name, var):
        assert isinstance(name, str), type(name)
        ret = OpDef.query_var(name)
        assert not ret, "name: {} already in var_map".format(name)
        OpDef.var_map[name] = var

    @staticmethod
    def query_null():
        return OpDef.null_op is not None

    @staticmethod
    def get_null():
        ret = OpDef.query_null()
        assert ret, "null op not exist in op def"
        null = OpDef.null_op
        return null

    @staticmethod
    def set_null(null):
        ret = OpDef.query_null()
        assert not ret, "null op already exist in op def"
        OpDef.null_op = null

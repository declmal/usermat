from fractions import Fraction

from .type_utils import cast_fraction


""" Op Definition Manager
"""
class OpDef(object):
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    var_map = {}
    sign_map = {}

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()
        OpDef.sign_map.clear()
        OpDef.var_map.clear()

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
        assert isinstance(op_id, int) and op_id >= 0, op_id
        assert op_id in OpDef.id_map, "invalid op_id: {}".format(op_id)
        op = OpDef.id_map[op_id]
        return op

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

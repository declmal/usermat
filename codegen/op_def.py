from fractions import Fraction

from .utils.type_utils import cast_fraction
from .utils.sign_utils import OpSign, merge_sign


""" Op Definition Manager
"""
class OpDef(object):
    current_id = 0
    equiv_map = {}
    id_map = {}
    sign_dict = {}

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
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
    def set_sign(op_id, sign):
        assert isinstance(op_id, int), type(op_id)
        ret = OpDef.query_sign(op_id)
        if not ret:
            OpDef.sign_dict[op_id] = sign
        else:
            org_sign = OpDef.sign_dict[op_id]
            nsign = merge_sign(org_sign, sign)
            OpDef.sign_dict[op_id] = nsign

    @staticmethod
    def get_sign_dict():
        return OpDef.sign_dict

    @staticmethod
    def query_equiv(equiv):
        if equiv in OpDef.equiv_map:
            return True
        return False

    @staticmethod
    def get_equiv(equiv):
        ret = OpDef.query_equiv(equiv)
        assert ret, "equiv: {} not in equiv_map".format(equiv)
        op = OpDef.equiv_map[equiv]
        return op

    @staticmethod
    def set_equiv(equiv, op):
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

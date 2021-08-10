from codegen.op_utils import cast_fraction


""" Op Definition Manager
"""
class OpDef(object):
    current_id = 0
    equiv_map = {}
    id_map = {}
    scalar_map = {}
    sign_map = {}

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

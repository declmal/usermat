import logging

import mxnet as mx

from codegen.sign_utils import infer_scalar_sign, merge_sign
from codegen.op_utils import Zero, One, FloatTypes, cast_fraction
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org
from codegen.base import Op


""" ops
"""
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_display")
@org.register_opt("dfs_info")
@org.register_opt("topo_fuse")
@org.register_opt("topo_standardize")
@org.register_opt("revtopo_infer_sign")
@org.register_op()
class Var(Op):
    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        return cls.default_op(*deps)

    def dfs_forward(self, val_dict):
        pass

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        cdiff[var_seq[cop_id]] = od.scalar(One)
        val_dict[cop_id] = cdiff


@org.register_opt("revtopo_infer_sign")
@org.register_op()
class Scalar(Op):
    def __init__(self, data):
        assert isinstance(data, FloatTypes)
        self.data = cast_fraction(data)
        super().__init__()

    def dfs_forward(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        val_dict[cop_id] = self.data

    def dfs_tosym(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        name = self.info(with_data=True)
        sym = mx.sym.var(name=name)
        val_dict[cop_id] = sym

    def dfs_display(
        self, val_dict, logger=logging.getLogger("op_info")):
        _info = self.info(with_data=True)
        logger.debug(_info)

    def dfs_info(self, val_dict):
        _info = self.info(with_data=True)
        op_id = self.id
        val_dict[op_id] = _info

    def dfs_autograph_backward(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        val_dict[cop_id] = cdiff

    def dfs_infer_sign(self, val_dict):
        data = self.data
        sign = infer_scalar_sign(data)
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            csign = merge_sign(sign, csign)
        val_dict[cop_id] = sign


@org.register_op()
class Null(Op):
    pass

import logging

import mxnet as mx

from ..utils.sign_utils import infer_scalar_sign, merge_sign
from ..utils.type_utils import (
    Zero, One, FloatTypes, cast_fraction, cast_float
)
from ..op_def import OpDef as od
from ..op_reg import OpReg as org

""" equivalent functions
"""
def scalar_valid_func(data):
    assert isinstance(data, FloatTypes), \
        "data: {}, type of data: {}".format(data, type(data))

def var_valid_func(name):
    assert isinstance(name, str), \
        "name: {}, type of name: {}".format(name, type(name))

""" ops
"""
@org.register_opt("dfs_infer_sign")
@org.register_op()
class Null(org.get_op_cls("op")):
    pass


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_ast")
@org.register_opt("revtopo_infer_sign")
@org.register_op(
    valid_func=scalar_valid_func, equiv_func=lambda op_type, data: data)
class Scalar(org.get_op_cls("op")):
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
        data = cast_float(self.data)
        sym_name = self.info(ext=data)
        sym = mx.sym.var(name=sym_name)
        val_dict[cop_id] = sym

    def dfs_display(
        self, val_dict, logger=logging.getLogger("op_info")):
        data = cast_float(self.data)
        _info = self.info(ext=data)
        logger.debug(_info)

    def dfs_info(self, val_dict):
        data = cast_float(self.data)
        _info = self.info(ext=data)
        op_id = self.id
        val_dict[op_id] = _info

    def dfs_autodiff(self, val_dict, var_seq):
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


@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_sort_deps")
@org.register_opt("topo_fuse")
@org.register_opt("topo_standardize")
@org.register_opt("revtopo_infer_sign")
@org.register_op(
    valid_func=var_valid_func, equiv_func=lambda op_type, name: name)
class Var(org.get_op_cls("op")):
    def __init__(self, name):
        super().__init__()
        self.name = name

    def dfs_tosym(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        sym_name = self.info(ext=self.name)
        sym = mx.sym.var(name=sym_name)
        val_dict[cop_id] = sym

    def dfs_display(
        self, val_dict, logger=logging.getLogger("op_info")):
        _info = self.info(ext=self.name)
        logger.debug(_info)

    def dfs_info(self, val_dict):
        _info = self.info(ext=self.name)
        op_id = self.id
        val_dict[op_id] = _info

    def dfs_forward(self, val_dict):
        pass

    def dfs_autodiff(self, val_dict, var_seq):
        cop_id = self.id
        assert cop_id not in val_dict
        cdiff = [od.scalar(Zero)] * len(var_seq)
        cdiff[var_seq[cop_id]] = od.scalar(One)
        val_dict[cop_id] = cdiff

    def dfs_ast(self, val_dict, variables, codeblocks):
        var_name = self.name
        variables.append(var_name)

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        return cls.default_op(*deps)

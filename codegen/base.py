import logging

import mxnet as mx

from codegen.sign_utils import OpSign, merge_sign
from codegen.op_utils import cast_float
from codegen.op_def import OpDef as od
from codegen.op_reg import OpReg as org

""" base Op
"""
@org.register_supported_opts
class Op(object):
    op_type = None
    op_equiv_func = None

    def __init__(self, *deps):
        self.deps = list(deps)
        self.id = -1

    def insert_assertion(self, sign):
        csign = self.sign
        sign = merge_sign(sign, csign)
        self.sign = sign

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
        od_func = getattr(od, cls.op_type)
        return od_func(*deps)

    @classmethod
    def topo_standardize(cls, sign_dict, *deps):
        return cls.default_op(*deps)

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        flag = True
        datas = []
        for dep in deps:
            if not isinstance(dep, org.get_op_cls("scalar")):
                flag = False
                break
            data = dep.data
            datas.append(data)
        if not flag:
            return cls.default_op(*deps)
        cdata = cls.fwd_func(*datas)
        op = od.scalar(cdata)
        return op

    @classmethod
    def topo_fuse(cls, sign_dict, *deps):
        return cls.default_op(*deps)

    def revtopo_infer_sign(self, sign_dict):
        pass

    def dfs_forward(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        vs = []
        for dep in self.deps:
            dep_id = dep.id
            dep_v = val_dict[dep_id]
            vs.append(dep_v)
        v = self.__class__.fwd_func(*vs)
        val_dict[cop_id] = v

    def dfs_display(
        self, val_dict,
        logger=logging.getLogger("op_info"), with_data=True):
        _info = self.info(with_data=with_data)
        logger.debug(_info)

    def dfs_tosym(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        name = self.info(with_data=False)
        dep_syms = []
        for dep in self.deps:
            dep_id = dep.id
            dep_sym = val_dict[dep_id]
            dep_syms.append(dep_sym)
        if len(dep_syms) == 0:
            sym = mx.sym.var(name=name)
        else:
            sym = mx.sym.add_n(*dep_syms, name=name)
        val_dict[cop_id] = sym

    def dfs_infer_sign(self, val_dict):
        sign = OpSign.UNDEFINED
        cop_id = self.id
        if cop_id in val_dict:
            csign = val_dict[cop_id]
            sign = merge_sign(sign, csign)
        val_dict[cop_id] = sign

    def dfs_autograph_backward(self, val_dict, var_seq):
        raise NotImplementedError

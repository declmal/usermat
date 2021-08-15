import logging

import mxnet as mx

from .sign_utils import OpSign, merge_sign
from .op_def import OpDef as od
from .op_reg import OpReg as org

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

    def info(self, ext=None):
        deps_info = ""
        if self.deps:
            deps_info = "deps:" + \
                ",".join([str(dep.id) for dep in self.deps])
        ext_info = ""
        if ext is not None:
            ext_info = "ext:{}".format(ext)
        s = "id:{},op_type:{}".format(self.id, self.op_type)
        _info = ",".join([s, ext_info, deps_info])
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
        # TODO: OpSign.ZERO degenerate
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
        self, val_dict, logger=logging.getLogger("op_info")):
        _info = self.info()
        logger.debug(_info)

    def dfs_info(self, val_dict):
        _info = self.info()
        op_id = self.id
        val_dict[op_id] = _info

    def dfs_tosym(self, val_dict):
        cop_id = self.id
        assert cop_id not in val_dict
        sym_name = self.info()
        dep_syms = []
        for dep in self.deps:
            dep_id = dep.id
            dep_sym = val_dict[dep_id]
            dep_syms.append(dep_sym)
        if len(dep_syms) == 0:
            sym = mx.sym.var(name=sym_name)
        else:
            sym = mx.sym.add_n(*dep_syms, name=sym_name)
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

    def __lt__(self, other):
        n = len(self.deps)
        n1 = len(other.deps)
        if n < n1:
            return True
        if n > n1:
            return False
        op_type = self.op_type
        priority = org.get_priority(op_type)
        op_type1 = other.op_type
        priority1 = org.get_priority(op_type1)
        if priority < priority1:
            return True
        if priority > priority1:
            return False
        if op_type == org.get_op_cls("scalar"):
            if self.data < other.data:
                return True
            return False
        if op_type == org.get_op_cls("var"):
            pass
        for i in range(n):
            dep = self.deps[i]
            dep1 = other.deps[i]
            if dep < dep1:
                return True
            if dep > dep1:
                return False
        return False

    def __gt__(self, other):
        pass

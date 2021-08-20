from ..utils.type_utils import Zero, ContradictError
from ..utils.sign_utils import (
    merge_sign, infer_nomorethan, infer_lessthan, OpSign, infer_notequal
)
from ..op_def import OpDef as od
from ..op_reg import OpReg as org
from ..base import Op
from .op_utils import num_valid_func, sequential_equiv_func


""" ops
"""
@org.register_opt("dfs_sort_deps")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNegative(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 < 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), type(op_type)
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertnotzero"),
                org.get_op_cls("assertnonpositive"))):
            return self
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if infer_nomorethan(OpSign.ZERO, dep_sign):
            raise ContradictError
        if infer_lessthan(dep_sign, OpSign):
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.NEGATIVE)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od.set_sign(dep_id, OpSign.NEGATIVE)
        op = super().topo_zerify(sign_dict, *deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.NEGATIVE)
        sign_dict[dep_id] = sign


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertPositive(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 > 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), op_type
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertnotzero"),
                org.get_op_cls("assertnonnegative"))):
            return self
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if infer_nomorethan(dep_sign, OpSign.ZERO):
            raise ContradictError
        if infer_lessthan(OpSign.ZERO, dep_sign):
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.POSITIVE)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od.set_sign(dep_id, OpSign.POSITIVE)
        op = super().topo_zerify(sign_dict, *deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.POSITIVE)
        sign_dict[dep_id] = sign


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_infer_sign")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNonPositive(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 <= 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), op_type
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertzero"),
                org.get_op_cls("assertnegative"))):
            return other
        if isinstance(other, org.get_op_cls("assertnonzero")):
            op = od.assertnegative(dep)
            return op
        if isinstance(other, org.get_op_cls("assertnonnegative")):
            op = od.assertzero(dep)
            return op
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if infer_lessthan(OpSign.ZERO, dep_sign):
            raise ContradictError
        if infer_nomorethan(dep_sign, OpSign.ZERO):
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.NON_POSITIVE)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od.set_sign(dep_id, OpSign.NON_POSITIVE)
        op = super().topo_zerify(sign_dict, *deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.NON_POSITIVE)
        sign_dict[dep_id] = sign


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_forward")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNonNegative(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 >= 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), op_type
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertzero"),
                org.get_op_cls("assertpositive"))):
            return other
        if isinstance(other, org.get_op_cls("assertnonzero")):
            op = od.assertpositive(dep)
            return op
        if isinstance(other, org.get_op_cls("assertnonpositive")):
            op = od.assertzero(dep)
            return op
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if infer_lessthan(dep_sign, OpSign.ZERO):
            raise ContradictError
        if infer_nomorethan(OpSign.ZERO, dep_sign):
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.NON_NEGATIVE)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od.set_sign(dep_id, OpSign.NON_NEGATIVE)
        op = super().topo_zerify(sign_dict, *deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.NON_NEGATIVE)
        sign_dict[dep_id] = sign


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_infer_sign")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_forward")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertNonZero(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 != 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), op_type
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertnegative"),
                org.get_op_cls("assertpositive"))):
            return other
        if isinstance(other, org.get_op_cls("assertnonnegative")):
            op = od.assertpositive(dep)
            return op
        if isinstance(other, org.get_op_cls("assertnonpositive")):
            op = od.assertnegative(dep)
            return op
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if dep_sign == OpSign.ZERO:
            raise ContradictError
        if infer_notequal(dep_sign, OpSign.ZERO):
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.NON_ZERO)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od.set_sign(dep_id, OpSign.NON_ZERO)
        op = super().topo_zerify(sign_dict, *deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.NON_ZERO)
        sign_dict[dep_id] = sign


@org.register_opt("dfs_sort_deps")
@org.register_opt("dfs_tosym")
@org.register_opt("dfs_infer_sign")
@org.register_op(
    valid_func=num_valid_func(1), equiv_func=sequential_equiv_func)
class AssertZero(Op):
    @classmethod
    def fwd_func(cls, v0):
        assert v0 == 0, v0
        return 1.0

    def merge(self, other):
        op_type = other.op_type
        assert op_type.startswith("assert"), op_type
        dep, odep = self.deps[0], other.deps[0]
        dep_id, odep_id = dep.id, odep.id
        assert dep_id == odep_id, \
            "dep_id: {}, odep_id: {}".format(dep_id, odep_id)
        if self == other:
            return self
        if isinstance(
            other, (org.get_op_cls("assertnonnegative"),
                org.get_op_cls("assertnonpositive"))):
            return self
        raise ContradictError

    @classmethod
    def topo_degenerate(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        dep_sign = od.get_sign(dep_id)
        if infer_notequal(dep_sign, OpSign.ZERO):
            raise ContradictError
        if dep_sign == OpSign.ZERO:
            op = od.null()
            return op
        od.set_sign(dep_id, OpSign.ZERO)
        op = cls.default_op(*deps)
        return op

    @classmethod
    def topo_zerify(cls, sign_dict, *deps):
        dep = deps[0]
        dep_id = dep.id
        od_sign = od.get_sign(dep_id)
        if od_sign == OpSign.ZERO:
            zero = od.scalar(Zero)
            ndeps = [zero]
            op = cls.default_op(*ndeps)
            return op
        od.set_sign(dep_id, OpSign.ZERO)
        op = cls.default_op(*deps)
        return op

    def revtopo_infer_sign(self, sign_dict):
        dep = self.deps[0]
        dep_id = dep.id
        dep_sign = sign_dict[dep_id]
        sign = merge_sign(dep_sign, OpSign.ZERO)
        sign_dict[dep_id] = sign

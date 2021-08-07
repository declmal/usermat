from typing import Dict, List, Callable, Optional, Union, Any, Set
import logging
from fractions import Fraction
from enum import Enum, auto

import numpy as np
import mxnet as mx

""" Types
"""
Float = Union[
    "int", "np.int32", "np.int64", "float",
    "np.float32", "np.float64", "Fraction"]
GradFuncType = Callable[["np.float64"], "np.float64"]
FwdFuncType = Callable[[List["np.float64"]], "np.float64"]
EquivFuncType = Callable[[str, List["Op"]], List[str]]
OpEquivFuncType = Callable[[List["Op"]], List[str]]
""" equivalent functions
"""
default_equiv_func: "EquivFuncType" = lambda op_type, ops: []
sequential_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join([str(op.id) for op in ops]))]
swappable_equiv_func: "EquivFuncType" = \
    lambda op_type, ops: [
        "{}:[{}]".format(op_type, ",".join(
            sorted([str(op.id) for op in ops])))]
""" Op Sign Types
"""
class OpSign(Enum):
    NON_NEGATIVE = auto()
    POSITIVE = auto()
    NON_POSITIVE = auto()
    NEGATIVE = auto()
    NON_ZERO = auto()
    ZERO = auto()
    INDEFINITE = auto()

def infer_negative_sign(sign: "OpSign") -> "OpSign":
    if sign == OpSign.POSITIVE:
        return OpSign.NEGATIVE
    if sign == OpSign.NEGATIVE:
        return OpSign.POSITIVE
    if sign == OpSign.NON_NEGATIVE:
        return OpSign.NON_POSITIVE
    if sign == OpSign.NON_POSITIVE:
        return OpSign.NON_NEGATIVE
    return sign

def infer_add_sign(x_sign: "OpSign", y_sign: "OpSign") -> "OpSign":
    lst1 = [OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    lst2 = [OpSign.NON_POSITIVE, OpSign.NEGATIVE]
    if x_sign == OpSign.INDEFINITE or y_sign == OpSign.INDEFINITE:
        return OpSign.INDEFINITE
    if x_sign == OpSign.ZERO:
        return y_sign
    if y_sign == OpSign.ZERO:
        return x_sign
    if x_sign == OpSign.NON_ZERO or y_sign == OpSign.NON_ZERO:
        return OpSign.INDEFINITE
    if x_sign == y_sign:
        return x_sign
    if x_sign in lst1 and y_sign in lst1:
        return OpSign.POSITIVE
    if x_sign in lst2 and y_sign in lst2:
        return OpSign.NEGATIVE
    return OpSign.INDEFINITE

def infer_abs_sign(sign: "OpSign") -> "OpSign":
    if sign in [OpSign.NON_ZERO, OpSign.POSITIVE, OpSign.NEGATIVE]:
        return OpSign.POSITIVE
    return OpSign.NON_NEGATIVE

def infer_multiply_sign(x_sign: "OpSign", y_sign: "OpSign") -> "OpSign":
    if x_sign == OpSign.ZERO or y_sign == OpSign.ZERO:
        return OpSign.ZERO
    if x_sign == OpSign.INDEFINITE or y_sign == OpSign.INDEFINITE:
        return OpSign.INDEFINITE
    lst1 = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
    if x_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if y_sign in lst1 else OpSign.INDEFINITE
        return sign
    if y_sign == OpSign.NON_ZERO:
        sign = OpSign.NON_ZERO if x_sign in lst1 else OpSign.INDEFINITE
        return sign
    lst2 = [OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    if x_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if y_sign in lst2 else OpSign.NON_POSITIVE
        return sign
    if y_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if x_sign in lst2 else OpSign.NON_POSITIVE
        return sign
    if x_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if y_sign in lst2 else OpSign.NON_NEGATIVE
        return sign
    if y_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if x_sign in lst2 else OpSign.NON_NEGATIVE
        return sign
    sign = OpSign.POSITIVE if x_sign == y_sign else OpSign.NEGATIVE
    return sign

def infer_power_sign(frac_sign: "Opsign", exp_data: "Fraction") -> "OpSign":
    assert isinstance(exp_data, Fraction), exp_data
    nume, deno = exp_data.numerator, exp_data.denominator
    if nume < 0:
        assert frac_sign != OpSign.ZERO
    if deno > 1:
        assert frac_sign != OpSign.NEGATIVE
    if nume == 0:
        return OpSign.POSITIVE
    if nume % 2 == 0 or deno > 1:
        sign = infer_abs_sign(frac_sign) if nume > 0 else OpSign.POSITIVE
        return sign
    if frac_sign == OpSign.NON_NEGATIVE:
        sign = OpSign.NON_NEGATIVE if nume > 0 else OpSign.POSITIVE
        return sign
    if frac_sign == OpSign.NON_POSITIVE:
        sign = OpSign.NON_POSITIVE if nume > 0 else OpSign.NEGATIVE
        return sign
    return frac_sign

def infer_mutual_sign(x_sign: "OpSign", y_sign: "Opsign") -> "OpSign":
    if x_sign == y_sign:
        return x_sign
    lst1 = [OpSign.ZERO, OpSign.POSITIVE, OpSign.NON_NEGATIVE]
    if x_sign in lst1 and y_sign in lst1:
        return OpSign.NON_NEGATIVE
    lst2 = [OpSign.ZERO, OpSign.NEGATIVE, OpSign.NON_POSITIVE]
    if x_sign in lst2 and y_sign in lst2:
        return OpSign.NON_POSITIVE
    lst3 = [OpSign.POSITIVE, OpSign.NEGATIVE, OpSign.NON_ZERO]
    if x_sign in lst3 and y_sign in lst3:
        return OpSign.NON_ZERO
    return OpSign.INDEFINITE

def infer_lessthan(a_sign: "OpSign", b_sign: "OpSign") -> bool:
    if a_sign == OpSign.NEGATIVE and \
        b_sign in [Opsign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    if a_sign in [OpSign.NON_POSITIVE, OpSign.ZERO] and b_sign == OpSign.POSITIVE:
        return True
    return False

def infer_nomorethan(a_sign: "OpSign", b_sign: "OpSign") -> bool:
    if a_sign in [OpSign.ZERO, OpSign.NON_POSITIVE, OpSign.NEGATIVE] and \
        b_sign in [OpSign.ZERO, OpSign.NON_NEGATIVE, OpSign.POSITIVE]:
        return True
    return False

# Zero = np.float64(0.0)
# One = np.float64(1.0)
Zero = Fraction(0)
One = Fraction(1)
MinusOne = Fraction(-1)

def cast_float(scalar: "Float") -> "np.float64":
    if isinstance(scalar, np.float64):
        return scalar
    else:
        return np.float64(scalar)

def cast_fraction(scalar: "Float") -> "Fraction":
    if isinstance(scalar, Fraction):
        return scalar
    else:
        return Fraction(scalar)

supported_ops: Dict[str, type] = {}
op_supported_opts: Dict[str, Set[str]] = {}

def register_op(
    num_deps: Optional[int],
    equiv_func: "EquivFuncType"=default_equiv_func):
    def wrapper(cls):
        op_type: str = cls.__name__.lower()
        assert op_type not in supported_ops, \
            "op_type: {} has been registered".format(op_type)
        setattr(cls, "op_type", op_type)
        setattr(cls, "num_deps", num_deps)

        def op_equiv_func(ops: List["Op"]) -> List[str]:
            if num_deps is not None:
                assert len(ops) == num_deps, \
                    "invalid number of ops: {}, expected: {}".format(
                        len(ops), num_deps)
            return equiv_func(op_type, ops)

        setattr(cls, "op_equiv_func", op_equiv_func)
        supported_ops[op_type] = cls
        _op_supported_opts = op_supported_opts[op_type] = set()
        for k, v in cls.__dict__.items():
            if (k.startswith("topo_") or k.startswith("dfs_")) and \
                type(v).__name__ == "classmethod":
                _op_supported_opts.add(k)
        return cls
    return wrapper

def get_opt(op: "Op", callback: str) -> "Op":
    op_type = op.op_type
    assert op_type in supported_ops, \
        "Op: {} has not been registered".format(op_type)
    op_cls = supported_ops[op_type]
    _op_supported_opts = op_supported_opts[op_type]
    assert callback in _op_supported_opts, \
        "Op: {}, Opt: {} has not been registered".format(op_type, callback)
    opt_func = getattr(op_cls, callback)
    return opt_func


class Op(object):
    _grad_fns: List["GradFuncType"] = []
    op_type: Optional[str] = None
    num_deps: Optional[int] = None
    op_equiv_func: Optional["OpEquivFuncType"] = None
    fwd_func: FwdFuncType
    is_scalar: bool = False

    def __init__(self, *deps: "Op")-> None:
        if self.num_deps is not None:
            assert len(deps) == self.num_deps, \
                "invalid deps number: {}, ".format(len(deps)) + \
                "expected: {}".format(self.num_deps)
        self.deps: List["Op"] = list(deps)
        self.data: "Float" = cast_fraction(0)
        self.grad: "Float" = cast_fraction(0)
        self.id: int = -1
        self.diff: List["Op"] = []
        self.sym: Optional["mx.sym.Symbol"] = None

    def set_id(self, op_id: int) -> None:
        self.id = op_id

    def set_data(self, data: "Float") -> None:
        self.data = data

    def get_sign(self) -> "OpSign":
        op_id = self.id
        sign = OpDef.get_sign(op_id)
        return sign

    def infer_sign(self) -> "OpSign":
        return OpSign.INDEFINITE

    @classmethod
    def default_op(cls, *deps: "Op") -> "Op":
        od_func = getattr(OpDef, cls.op_type)
        return od_func(*deps)

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    @classmethod
    def topo_toscalar(cls, *deps: "Op") -> "Op":
        flag = True
        datas = []
        for dep in deps:
            if not isinstance(dep, Scalar):
                flag = False
                break
            data = dep.data
            datas.append(data)
        if not flag:
            return cls.default_op(*deps)
        cdata = cls.fwd_func(*datas)
        op = OpDef.scalar(cdata)
        return op

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    @classmethod
    def topo_validate(cls, *deps: "Op") -> "Op":
        pass

    def reset(self) -> None:
        self.data = cast_fraction(0)
        self.diff.clear()
        self.sym = None

    def forward(self) -> None:
        vs: List["np.float64"] = \
            [np.float64(dep.data) for dep in self.deps]
        self.data = self.__class__.fwd_func(*vs)

    def info(self, with_data=True) -> str:
        deps_info = ""
        if self.deps:
            deps_info = "deps:" + \
                ",".join([str(dep.id) for dep in self.deps])
        data_info = ""
        if with_data:
            data_info = "data:{}".format(cast_float(self.data))
        s = "id:{},op_type:{}".format(self.id, self.op_type)
        return ",".join([s, data_info, deps_info])

    def display(self, logger=logging.getLogger("op_info")) -> None:
        logger.debug(self.info())

    def to_sym(self) -> None:
        name = self.info(with_data=False)
        dep_syms = [dep.sym for dep in self.deps]
        if not dep_syms:
            self.sym = mx.sym.var(name=name)
        else:
            self.sym = mx.sym.add_n(*dep_syms, name=name)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        raise NotImplementedError

    # TODO: deprecate
    def backward(self, grad: "Float"=cast_fraction(1)) -> None:
        grad = cast_fraction(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

supported_opts: Set[str] = { \
    k for k, v in Op.__dict__.items() \
    if k.startswith("topo_") and type(v).__name__ == "classmethod"
}

def register_opt(callback: str):
    assert callback in supported_opts, \
        "Opt: {} is not supported by Op".format(callback)

    def wrapper(cls):
        op_type = getattr(cls, "op_type")
        _op_supported_opts = op_supported_opts[op_type]
        assert callback not in _op_supported_opts, \
            "Op: {}, Opt: {} has been registered".format(op_type, callback)
        _op_supported_opts.add(callback)
        return cls
    return wrapper


@register_op(0)
class Scalar(Op):
    is_scalar: bool = True

    def infer_sign(self) -> "OpSign":
        assert self.data is not None, "run set_data first"
        if self.data == Zero:
            return OpSign.ZERO
        if self.data > Zero:
            return OpSign.POSITIVE
        return OpSign.NEGATIVE

    def forward(self):
        pass

    def reset(self) -> None:
        self.diff.clear()
        self.sym = None

    def to_sym(self) -> None:
        name = self.info()
        self.sym = mx.sym.var(name=name)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [OpDef.scalar(0)] * len(var_seq)


@register_opt("topo_fuse")
@register_opt("topo_degenerate")
@register_opt("topo_standardize")
@register_op(0)
class Var(Op):
    @classmethod
    def topo_toscalar(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    def forward(self):
        pass

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [OpDef.scalar(0)] * len(var_seq)
        self.diff[var_seq[self.id]] = OpDef.scalar(1)


class AssertExceedZeroError(Exception):
    pass


@register_op(1, equiv_func=swappable_equiv_func)
class AssertExceedZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        # TODO: unittest test_assert_exceed_zero.py
        if v <= Zero:
            raise AssertExceedZeroError("value: {}".format(v))
        return Zero


class AssertNotZeroError(Exception):
    pass


@register_op(1, equiv_func=swappable_equiv_func)
class AssertNotZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        # TODO: unittest test_assert_not_zero.py
        if v == Zero:
            raise AssertNotZeroError("value: {}".format(v))
        return Zero


class AssertNoLessThanZeroError(Exception):
    pass


@register_op(1, equiv_func=sequential_equiv_func)
class AssertNoLessThanZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        if v < Zero:
            raise AssertNoLessThanZeroError("value: {}".format(v))
        return Zero


@register_op(None, equiv_func=sequential_equiv_func)
class Polynomial(Op):
    def infer_sign(self) -> "OpSign":
        dep_signs = [dep.get_sign() for dep in self.deps]
        signs = [dep_signs[0]]
        for i in range(1, len(dep_signs), 2):
            x_sign = dep_signs[i]
            y_sign = dep_signs[i+1]
            sign = infer_multiply_sign(x_sign, y_sign)
            signs.append(sign)
        sign = signs[0]
        for cur_sign in signs[1:]:
            sign = infer_add_sign(sign, cur_sign)
        return sign

    @classmethod
    def fwd_func(cls, *v: "Float") -> "Float":
        summation = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                summation += v[i]
            else:
                summation += v[i]*v[i+1]
        return summation


@register_op(None, equiv_func=sequential_equiv_func)
class Monomial(Op):
    def infer_sign(self) -> "OpSign":
        dep_signs = [dep.get_sign() for dep in self.deps]
        signs = [dep_signs[0]]
        for i in range(1, len(dep_signs), 2):
            frac_sign = dep_signs[i]
            exp = self.deps[i+1]
            exp_data = exp.data
            sign = infer_power_sign(frac_sign, exp_data)
            signs.append(sign)
        sign = signs[0]
        for cur_sign in signs[1:]:
            sign = infer_multiply_sign(sign, cur_sign)
        return sign

    @classmethod
    def fwd_func(cls, *v: "Float") -> "Float":
        product = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                product *= v[i]
            else:
                product *= v[i]**v[i+1]
        return product

    def autograph_backward(self, var_seq: Dict[int, int]) -> None:
        raise NotImplementedError

def get_monomial_dict(op: "Op") -> Dict[int,"Float"]:
    if isinstance(op, Monomial):
        deps = op.deps
        ret = {-1: deps[0].data}
        for i in range(1, len(deps), 2):
            dep_id = deps[i].id
            assert dep_id not in ret
            exp_data = deps[i+1].data
            assert isinstance(exp_data, Fraction)
            ret[dep_id] = exp_data
        return ret
    elif isinstance(op, Scalar):
        ret = {-1: op.data}
        return ret
    else:
        ret = {-1: One}
        op_id = op.id
        assert op_id not in ret
        ret[op_id] = Fraction(One)
        return ret

def merge_monomial_dict(
    dict1: Dict[int,"Float"],
    dict2: Dict[int,"Float"]) -> Dict[int,"Float"]:
    ret = dict2
    for op_id, scalar_data in dict1.items():
        if op_id == -1:
            ret[-1] *= scalar_data
            if ret[-1] == Zero:
                return {-1: Zero}
            continue
        assert isinstance(scalar_data, Fraction)
        if op_id not in ret:
            ret[op_id] = scalar_data
            continue
        scalar_data2 = ret[op_id]
        assert isinstance(scalar_data2, Fraction)
        if scalar_data2 + scalar_data == Zero:
            del ret[op_id]
        else:
            ret[op_id] = scalar_data + scalar_data2
    return ret

def get_polynomial_dict(op: "Op") -> Dict[int,"Float"]:
    if isinstance(op, Polynomial):
        deps = op.deps
        ret = {-1: deps[0].data}
        for i in range(1, len(deps), 2):
            dep_id = deps[0].id
            assert dep_id not in ret
            coef_data = deps[i+1].data
            ret[dep_id] = coef_data
        return ret
    elif isinstance(op, Scalar):
        ret = {-1: op.data}
        return ret
    else:
        ret = {-1: Zero}
        op_id = op.id
        assert op_id not in ret
        ret[op_id] = One
        return ret

def merge_polynomial_dict(
    dict1: Dict[int,"Float"],
    dict2: Dict[int,"Float"]) -> Dict[int,"Float"]:
    ret = dict2
    for op_id, scalar_data in dict1.items():
        if op_id == -1:
            ret[-1] += scalar_data
            continue
        if op_id not in ret:
            ret[op_id] = scalar_data
        elif ret[op_id] + scalar_data == Zero:
            del ret[op_id]
        else:
            ret[op_id] += scalar_data
    return ret

def create_mial_op(m_dict: Dict[int,"Float"], op_type: str) -> "Op":
    assert op_type in ["polynomial", "monomial"], op_type
    assert -1 in m_dict, m_dict.keys()
    scalar_data = m_dict[-1]
    assert scalar_data != Zero, scalar_data
    id_data = [
        (op_id, data) for op_id, data in m_dict.items() if op_id != -1]
    for _, data in id_data:
        assert data != Zero, data
    if len(id_data) == 1 and id_data[0][1] == One:
        if op_type == "polynomial" and scalar_data == Zero or \
            op_type == "monomial" and scalar_data == One:
            op_id = id_data[0][0]
            op = OpDef.get_op(op_id)
            return op
    id_data.sort()
    scalar = OpDef.scalar(scalar_data)
    deps = [scalar]
    for op_id, data in id_data:
        if op_type == "monomial":
            assert isinstance(data, Fraction), type(data)
        cop = OpDef.get_op(op_id)
        deps.append(cop)
        scalar = OpDef.scalar(data)
        deps.append(scalar)
    od_func = getattr(OpDef, op_type)
    op = od_func(*deps)
    return op


@register_op(1, equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func: FwdFuncType = lambda v: -v

    def infer_sign(self) -> "OpSign":
        dep_sign = self.deps[0].get_sign()
        sign = infer_negative_sign(dep_sign)
        return sign

    @classmethod
    def topo_standardize(cls, deps: "Op") -> "Op":
        minus_one = OpDef.scalar(-1)
        x = deps
        op = OpDef.multiply(x, minus_one)
        return op


@register_opt("topo_fuse")
@register_opt("topo_toscalar")
@register_opt("topo_degenerate")
@register_opt("topo_standardize")
@register_op(1, equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func: FwdFuncType = lambda v: np.sin(v)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x = self.deps[0]
        y = OpDef.cos(x)
        self.diff.clear()
        for di in x.diff:
            dop = OpDef.multiply(y, di)
            self.diff.append(dop)


@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_opt("topo_degenerate")
@register_opt("topo_fuse")
@register_op(1, equiv_func=sequential_equiv_func)
class Abs(Op):
    fwd_func: FwdFuncType = lambda v: np.abs(v)

    def infer_sign(self) -> "OpSign":
        dep_sign = self.deps[0].get_sign()
        sign = infer_abs_sign(dep_sign)
        return sign

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x = self.deps[0]
        self.diff.clear()
        for di in x.diff:
            zero_op = OpDef.scalar(0)
            neg_op = OpDef.negative(di)
            dop = OpDef.lessthan(di, zero_op, neg_op, di)
            self.diff.append(dop)


@register_opt("topo_fuse")
@register_op(1, equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func: FwdFuncType = lambda v: np.cos(v)


@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]
    fwd_func: FwdFuncType = lambda v0, v1: v0 + v1

    def infer_sign(self) -> "OpSign":
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_add_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        x, y = deps
        x_dict = get_polynomial_dict(x)
        y_dict = get_polynomial_dict(y)
        m_dict = merge_polynomial_dict(x_dict, y_dict)
        scalar = OpDef.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = OpDef.get_op(op_id)
            ndeps.append(dep)
            coef = OpDef.scalar(scalar_data)
            ndeps.append(coef)
        if len(ndeps) == 3 and ndeps[0].data == Zero and \
            ndeps[2].data == One:
            return ndeps[1]
        op = OpDef.polynomial(*ndeps)
        return op

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero:
            return y
        if isinstance(y, Scalar) and y.data == Zero:
            return x
        return cls.default_op(*deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(var_seq)):
            dop = OpDef.add(d0[i], d1[i])
            self.diff.append(dop)


@register_op(2, equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 - v1

    def infer_sign(self) -> "OpSign":
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_negative_sign(y_sign)
        sign = infer_add_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> "Op":
        x, y = deps
        minus_one = OpDef.scalar(-1)
        minus_y = OpDef.multiply(minus_one, y)
        op = OpDef.add(x, minus_y)
        return op


@register_opt("topo_standardize")
@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 * v1

    def infer_sign(self) -> "OpSign":
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        sign = infer_multiply_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        x, y = deps
        x_dict = get_monomial_dict(x)
        y_dict = get_monomial_dict(y)
        m_dict = merge_monomial_dict(x_dict, y_dict)
        scalar = OpDef.scalar(m_dict[-1])
        if len(m_dict) == 1:
            return scalar
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            dep = OpDef.get_op(op_id)
            ndeps.append(dep)
            exp = OpDef.scalar(scalar_data)
            ndeps.append(exp)
        if len(ndeps) == 3 and ndeps[0].data == One and \
            ndeps[2].data == One:
            return ndeps[1]
        op = OpDef.monomial(*ndeps)
        return op

    @classmethod
    def topo_toscalar(cls, *deps: "Op") -> "Op":
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero or \
            isinstance(y, Scalar) and y.data == Zero:
            return OpDef.scalar(0)
        return cls.default_op(*deps)

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        x, y = deps
        if isinstance(x, Scalar) and x.data == One:
            return y
        if isinstance(y, Scalar) and y.data == One:
            return x
        return cls.default_op(*deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(d0)):
            mul0 = OpDef.multiply(x1, d0[i])
            mul1 = OpDef.multiply(x0, d1[i])
            dop = OpDef.add(mul0, mul1)
            self.diff.append(dop)

def validate_exp(frac_data: "Float", exp_data: "Fraction") -> None:
    if frac_data == Zero:
        assert exp_data >= Zero, \
            "zero division occurs: frac_data: {}".format(
                frac_data) + ", exp_data: {}".format(exp_data)
    elif frac_data < Zero:
        assert exp_data.denominator % 2 == 1, \
            "the denominator of exp must be odd for negative fraction," + \
            " frac_data: {}, exp_data: {}".format(frac_data, exp_data)


class ExpContradictError(Exception):
    pass


@register_opt("topo_standardize")
@register_op(2, equiv_func=sequential_equiv_func)
class Power(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0**v1

    def infer_sign(self) -> "OpSign":
        frac, exp = self.deps
        exp_data = exp.data
        frac_sign = frac.get_sign()
        sign = infer_power_sign(frac_sign, exp_data)
        return sign

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        frac, exp = deps
        exp_data = exp.data
        assert isinstance(exp_data, Fraction) and \
            exp_data != One and exp_data != Zero, \
            "invalid exp_data: {}, ".format(exp_data) + \
            "run degenerate and toscalar first"
        m_dict = get_monomial_dict(frac)
        if len(m_dict) == 1:
            data = m_dict[-1]
            validate_exp(data, exp_data)
            scalar_data = data ** exp_data
            scalar = OpDef.scalar(scalar_data)
            return scalar
        assert m_dict[-1] != Zero, m_dict[-1]
        deno, nume = exp_data.denominator, exp_data.numerator
        if nume < 0:
            # in case that: exp < 0
            # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
            # for op with op_id as i_n
            # validate op is not equal to zero
            for op_id, data in m_dict.items():
                if op_id == -1:
                    assert data != Zero
                    continue
                assert isinstance(data, Fraction)
                deno_in, nume_in = data.denominator, data.numerator
                if nume_in >= 0:
                    continue
                op = OpDef.get_op(op_id)
                OpDef.assertnotzero(op)
        if deno % 2 == 0 or deno > 1:
            # in case that: exp = Fraction(odd_num, even_num)
            # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
            # m_dict will first be decomposed into nm_dict and sm_dict
            # where
            # nm_dict: {-1: abs(scalar_data), j1: f1, j2: f2, ... }
            # where fn = Fraction(odd_num, even_num) or 
            #            Fraction(even_num, odd_num) or
            #            Fraction(odd_num1, odd_num2>1)
            # sm_dict: {-1, sgn(scalar_data), k1: g1, k2: g2, ... }
            # where gn = Fraction(odd_num, 1)
            nm_dict = {}
            sm_dict = {}
            for op_id, data in m_dict.items():
                if op_id == -1:
                    if data > Zero:
                        nm_dict[-1] = data
                        sm_dict[-1] = One
                    else:
                        nm_dict[-1] = -data
                        sm_dict[-1] = MinusOne
                    continue
                deno_in, nume_in = data.denominator, data.numerator
                cop = OpDef.get_op(op_id)
                if nume_in % 2 == 0 or deno_in % 2 == 0 or \
                    isinstance(cop, Abs) or deno_in > 1:
                    if nume_in % 2 == 0:
                        assert not isinstance(cop, Abs), type(cop)
                    nm_dict[op_id] = data
                    continue
                sm_dict[op_id] = data
            if len(sm_dict) > 1:
                OpDef.assertexceedzero(frac)
                frac_id = frac.id
                m_dict = {-1: One, frac_id: One}
            else:
                assert -1 in sm_dict, sm_dict.keys()
                scalar_data = sm_dict[-1]
                if scalar_data < Zero:
                    # unittest test_power_3.py
                    raise ExpContradictError(
                        "contradictory exp_data: {}, ".format(exp_data) + \
                        "dep_ids: {}".format([dep.id for dep in self.deps]))
                m_dict = nm_dict
        # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
        # exp = Fraction(nume, deno)
        # in case that en = Fraction(even_num, odd_num)
        # and deno = k * even_num
        # the op with op_id as i_n should be turned into abs(op)
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                continue
            assert isinstance(data, Fraction), data
            nume_in = data.numerator
            if nume_in % 2 == 0 and deno >= nume_in and \
                deno % nume_in == 0:
                del m_dict[op_id]
                cop = OpDef.get_op(op_id)
                nop = OpDef.abs(cop)
                nid = nop.id
                if nid not in m_dict:
                    m_dict[nid] = data
                else:
                    # unittest test_power_2.py
                    m_dict[nid] += data
                    assert isinstance(m_dict[nid], Fraction), m_dict[nid]
        # update m_dict by power
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                scalar_data = data ** exp_data
                m_dict[-1] = scalar_data
                continue
            ndata = data * exp_data
            assert isinstance(ndata, Fraction), ndata
            m_dict[op_id] = ndata
        # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
        # in case that en = Fraction(2*k, deno_in)
        # and op with op_id i_n is abs(sop)
        # then op should be turned into sop
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                continue
            assert isinstance(data, Fraction), data
            nume_in = data.numerator
            op = OpDef.get_op(op_id)
            if nume_in % 2 == 0 and isinstance(op, Abs):
                del m_dict[op_id]
                sop = op.deps[0]
                sid = sop.id
                if sid not in m_dict:
                    m_dict[sid] = data
                else:
                    # unittest test_power_1.py
                    m_dict[sid] += data
                    assert isinstance(m_dict[sid], Fraction)
        # create monomial op
        op = create_mial_op(m_dict, "monomial")
        return op

    @classmethod
    def topo_toscalar(cls, *deps: "Op") -> None:
        x, y = deps
        if y.data == Zero:
            op = OpDef.scalar(1)
            return op
        if isinstance(x, Scalar):
            validate_exp(x.data, y.data)
        return cls.default_op(*deps)

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> None:
        x, y = deps
        if y.data == One:
            return x
        return cls.default_op(*deps)

    @classmethod
    def topo_validate(cls, *deps: "Op") -> None:
        frac, exp = deps
        assert isinstance(exp, Scalar), \
            "type of deps[1]: {} must be scalar".format(
                type(exp).__class__.__name__)
        exp_data = exp.data
        deno = exp_data.numerator
        if nume < 0:
            OpDef.assertnotzero(frac)
        deno = exp_data.denominator
        if deno > 1:
            sign = OpSign.NON_NEGATIVE
            frac.set_sign(sign)
            OpDef.assertexceedzero(frac)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x, y = self.deps
        d = x.diff
        assert any([dd is not None for dd in d])
        self.diff.clear()
        nscalar = OpDef.scalar(y.data-1)
        npower = OpDef.power(x, nscalar)
        mul_scalar = OpDef.multiply(y, npower)
        for i in range(len(d)):
            dop = OpDef.multiply(mul_scalar, d[i])
            self.diff.append(dop)


@register_op(2, equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 / v1

    def infer_sign(self) -> "OpSign":
        x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        ny_sign = infer_power_sign(y_sign, MinusOne)
        sign = infer_multiply_sign(x_sign, ny_sign)
        return sign

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> None:
        x, y = deps
        minus_one = OpDef.scalar(-1)
        _pow = OpDef.power(y, minus_one)
        op = OpDef.multiply(x, _pow)
        return op

def cnd_auto_backward(
    deps: List["Op"], od_func: Callable[[List["Op"]], "Op"],
    var_seq: Dict[int,int]) -> List["Op"]:
    lhs, rhs, lv, rv = deps
    dl, dr = lv.diff, rv.diff
    diff = []
    for i in range(len(dl)):
        dop = od_func(lhs, rhs, dl[i], dr[i])
        diff.append(dop)
    return diff

def cnd_topo_degenerate(
    default_op_func: Callable[[List["Op"]], "Op"], *deps: "Op") -> "Op":
    lhs, rhs, lv, rv = deps
    if lv.id == rv.id:
        return lv
    return default_op_func(*deps)

@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_opt("topo_fuse")
@register_op(4, equiv_func=default_equiv_func)
class LessThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def infer_sign(self) -> "OpSign":
        a_sign, b_sign, x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        if infer_lessthan(a_sign, b_sign):
            return x_sign
        if infer_nomorethan(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        return cnd_topo_degenerate(cls.default_op, *deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        od_func = getattr(OpDef, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)


@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_opt("topo_fuse")
@register_op(4, equiv_func=default_equiv_func)
class NoMoreThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    def infer_sign(self) -> "OpSign":
        a_sign, b_sign, x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        if infer_nomorethan(a_sign, b_sign):
            return x_sign
        if infer_lessthan(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        return cnd_topo_degenerate(cls.default_op, *deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        od_func = getattr(OpDef, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)

def register_op_def(cls):
    def scalar_func(data: "Float") -> "Op":
        nv: "Fraction" = cast_fraction(data)
        if nv in OpDef.scalar_map:
            return OpDef.scalar_map[nv]
        op: "Op" = Scalar()
        OpDef.set_id(op)
        op.set_data(nv)
        op_id = op.id
        OpDef.scalar_map[nv] = op
        OpDef.id_map[op.id] = op
        sign = op.infer_sign()
        OpDef.set_sign(op_id, sign)
        return op

    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            equivs: List[str] = op_cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op: "Op" = OpDef.equiv_map[equiv]
                    return equiv_op
            op: "Op" = op_cls(*deps)
            OpDef.set_id(op)
            for equiv in equivs:
                OpDef.equiv_map[equiv] = op
            op_id = op.id
            OpDef.id_map[op_id] = op
            op_type = getattr(op_cls, "op_type")
            if op_type.startswith("assert"):
                OpDef.set_assert(op_id)
            sign = op.infer_sign()
            OpDef.set_sign(op_id, sign)
            return op
        return wrapper

    for op_cls in supported_ops.values():
        op_type = getattr(op_cls, "op_type")
        if op_type == "scalar":
            setattr(cls, op_type, scalar_func)
        else:
            setattr(cls, op_type, op_func(op_cls))
    return cls


@register_op_def
class OpDef(object):
    current_id: int = 0
    equiv_map: Dict[str, "Op"] = {}
    id_map: Dict[int, "Op"] = {}
    scalar_map: Dict["Fraction", "Op"] = {}
    assert_set: Set[int] = set()
    sign_map: Dict[int, "OpSign"] = {}

    @staticmethod
    def get_assert_ops() -> List["Op"]:
        return list([
            OpDef.get_op(op_id) for op_id in OpDef.assert_set])

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()
        OpDef.assert_set.clear()
        OpDef.sign_map.clear()

    @staticmethod
    def set_sign(op_id: int, sign: "OpSign"):
        assert op_id not in OpDef.sign_map, \
            "duplicate op id: {}, sign_map: {}".format(op_id, OpDef.sign_map)
        OpDef.sign_map[op_id] = sign

    @staticmethod
    def get_sign(op_id: int) -> "OpSign":
        assert op_id in OpDef.sign_map, \
            "op id: {} not in sign_map: {}".format(op_id, OpDef.sign_map)
        return OpDef.sign_map[op_id]

    @staticmethod
    def set_assert(op_id: int):
        assert op_id not in OpDef.assert_set, \
            "duplicate op id: {}, assert_set: {}".format(op_id, OpDef.assert_set)
        OpDef.assert_set.add(op_id)


    @staticmethod
    def set_id(op: "Op") -> None:
        op.set_id(OpDef.current_id)
        OpDef.current_id += 1

    @staticmethod
    def get_op(op_id: int) -> None:
        assert op_id in OpDef.id_map, \
            "invalid op_id: {}".format(op_id)
        return OpDef.id_map[op_id]

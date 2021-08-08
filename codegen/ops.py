from typing import Dict, List, Callable, Optional, Union, Any, Set
import logging
from fractions import Fraction

import numpy as np
import mxnet as mx

import codegen.op_utils as outils
from codegen.op_utils import \
    FwdFuncType, sequential_equiv_func, swappable_equiv_func, \
    cast_fraction, OpSign, infer_power_sign, infer_multiply_sign, \
    infer_abs_sign, infer_negative_sign, infer_lessthan_sign, \
    infer_add_sign, infer_nomorethan_sign, infer_mutual_sign, \
    One, MinusOne, Zero, FloatTypes

""" validate functions
"""

def default_valid_func(*deps: "Op") -> None:
    return

def num_valid_func(num_deps) -> "ValidFuncType":
    def wrapper(*deps: "Op") -> None:
        assert len(deps) == num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
    return wrapper

def power_valid_func(*deps: "Op") -> None:
    validate_num_deps = num_valid_func(2)
    validate_num_deps(*deps)
    exp = deps[1]
    assert isinstance(exp, Scalar), \
        "type of deps[1]: {} must be scalar".format(type(exp))

def mial_valid_func(mial_type: "str") -> "ValidFuncType":
    assert mial_type in ["poly", "mono"]

    def wrapper(*deps: "Op") -> None:
        num_deps = len(deps)
        assert num_deps >= 1 and num_deps % 2 == 1, \
            "invalid number of deps: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
        for i in range(0, num_deps, 2):
            dep = deps[i]
            assert isinstance(dep, Scalar), \
                "invalid type of dep: {}".format(type(dep))
            data = dep.data
            assert isinstance(data, FloatTypes), \
                "invalid type of data: {}".format(type(data))
        if mial_type == "poly" or num_deps == 1:
            return
        for i in range(2, num_deps, 2):
            dep = deps[i]
            data = dep.data
            assert isinstance(data, Fraction), \
                "invalid type of data: {}".format(type(data))

    return wrapper

""" registration manager
"""

supported_ops: Dict[str, type] = {}
op_supported_opts: Dict[str, Set[str]] = {}

def register_op(
    valid_func: "ValidFuncType"=default_valid_func,
    equiv_func: "EquivFuncType"=outils.default_equiv_func):
    def wrapper(cls):
        op_type: str = cls.__name__.lower()
        assert op_type not in supported_ops, \
            "op_type: {} has been registered".format(op_type)
        setattr(cls, "op_type", op_type)

        def op_valid_func(init_func):
            def _wrapper(self, *deps: "Op") -> None:
                valid_func(*deps)
                init_func(self, *deps)
            return _wrapper

        init_func = getattr(cls, "__init__")
        init_func = op_valid_func(init_func)
        setattr(cls, "__init__", init_func)

        def op_equiv_func(ops: List["Op"]) -> List[str]:
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
    op_equiv_func: Optional["OpEquivFuncType"] = None
    fwd_func: FwdFuncType
    is_scalar: bool = False

    def __init__(self, *deps: "Op")-> None:
        self.deps: List["Op"] = list(deps)
        self.data: "Float" = cast_fraction(0)
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
    def topo_degenerate(cls, *deps: "Op") -> "Op":
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
    def topo_fuse(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

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


@register_op()
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
@register_opt("topo_standardize")
@register_op()
class Var(Op):
    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    def forward(self):
        pass

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [OpDef.scalar(0)] * len(var_seq)
        self.diff[var_seq[self.id]] = OpDef.scalar(1)


class AssertExceedZeroError(Exception):
    pass


@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
class AssertExceedZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        # TODO: unittest test_assert_exceed_zero.py
        if v <= Zero:
            raise AssertExceedZeroError("value: {}".format(v))
        return Zero


class AssertNotZeroError(Exception):
    pass


@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
class AssertNotZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        # TODO: unittest test_assert_not_zero.py
        if v == Zero:
            raise AssertNotZeroError("value: {}".format(v))
        return Zero


class AssertNoLessThanZeroError(Exception):
    pass


@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
class AssertNoLessThanZero(Op):
    @classmethod
    def fwd_func(cls, v: "Float") -> "Float":
        if v < Zero:
            raise AssertNoLessThanZeroError("value: {}".format(v))
        return Zero


@register_op(
    valid_func=mial_valid_func("poly"),
    equiv_func=sequential_equiv_func)
class Polynomial(Op):
    def infer_sign(self) -> "OpSign":
        dep_signs = [dep.get_sign() for dep in self.deps]
        signs = [dep_signs[0]]
        for i in range(1, len(dep_signs), 2):
            x_sign, y_sign = dep_signs[i:i+2]
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

    def autograph_backward(self, var_seq: Dict[int, int]) -> None:
        assert len(self.deps) > 1, "invoke degenerate first"
        scalar = self.deps[0]
        scalar_data = scalar.data
        if len(self.deps) == 3 and scalar_data == Zero:
            coef = self.deps[2]
            coef_data = coef.data
            assert coef_data != One, "invoke degenerate first"
        # create m_dict_ref
        m_dict_ref = {-1: scalar_data}
        for i in range(1, len(self.deps), 2):
            var, coef = self.deps[i:i+2]
            coef_data = coef.data
            assert coef_data != Zero, "invoke degenerate first"
            assert not isinstance(var, Scalar), "invoke degenerate first"
            var_id = var.id
            m_dict_ref[var_id] = coef_data
        validate_polynomial_dict(m_dict_ref)
        # differentials
        diff_map = {}
        for i in range(1, len(self.deps), 2):
            dep = self.deps[i]
            dep_id = dep.id
            diff = dep.diff
            diff_map[dep_id] = diff
        ns = {len(diff) for diff in diff_map.values()}
        assert len(ns) == 1, ns
        n = list(ns)[0]
        # backward propagation
        self.diff.clear()
        op_ids = list(diff_map.keys())
        for i in range(n):
            m_dict = {-1: Zero}
            for op_id in op_ids:
                diff_in = diff_map[op_id]
                di = diff_in[i]
                did = di.id
                scalar_data = m_dict_ref[op_id]
                m_dict[did] = scalar_data
            diff = create_polynomial_op(m_dict)
            self.diff.append(diff)

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            var, coef = deps[i:i+2]
            var_id = var.id
            coef_data = coef.data
            m_dict[var_id] = coef_data
        op = create_polynomial_op(m_dict)
        return op


@register_op(
    valid_func=mial_valid_func("mono"),
    equiv_func=sequential_equiv_func)
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
        assert len(self.deps) > 1, "invoke degenerate first"
        scalar = self.deps[0]
        scalar_data = scalar.data
        assert scalar_data != Zero, "invoke degenerate first"
        if len(self.deps) == 3 and scalar_data == One:
            exp = self.deps[2]
            exp_data = exp.data
            deno, nume = exp_data.denominator, exp_data.numerator
            assert deno != 1 or nume != 1, "invoke degenerate first"
        # create m_dict_ref
        m_dict_ref = {-1: scalar_data}
        for i in range(1, len(self.deps), 2):
            frac, exp = self.deps[i:i+2]
            exp_data = exp.data
            nume = exp_data.numerator
            assert nume != 0, "invoke degenerate first"
            assert not isinstance(frac, Scalar), "invoke degenerate first"
            frac_id = frac.id
            m_dict_ref[frac_id] = exp_data
        validate_monomial_dict(m_dict_ref)
        # create m_dict_dict
        m_dict_dict = {}
        for op_id, exp_data in m_dict_ref.items():
            if op_id == -1:
                continue
            m_dict = m_dict_ref.copy()
            deno, nume = exp_data.denominator, exp_data.numerator
            if deno == 1 and nume == 1:
                del m_dict[op_id]
            else:
                scalar_data = m_dict[-1]
                m_dict[-1] = scalar_data * exp_data
                nexp_data = exp_data - One
                m_dict[op_id] = nexp_data
            m_dict_dict[op_id] = m_dict.copy()
        # differentials
        diff_map = {}
        for i in range(1, len(self.deps), 2):
            dep = self.deps[i]
            dep_id = dep.id
            diff = dep.diff
            diff_map[dep_id] = diff
        ns = {len(diff) for diff in diff_map.values()}
        assert len(ns) == 1, ns
        n = list(ns)[0]
        # backward propagation
        self.diff.clear()
        op_ids = list(diff_map.keys())
        for i in range(n):
            m_dict = {-1: Zero}
            for op_id in op_ids:
                m_dict_partial = m_dict_dict[op_id]
                diff_in = diff_map[op_id]
                di = diff_in[i]
                m_dict_diff = get_monomial_dict(di)
                m_dict_in = merge_monomial_dict(
                    m_dict_partial, m_dict_diff)
                var = create_monomial_op(m_dict_in)
                var_id = var.id
                m_dict[var_id] = One
            diff = create_polynomial_op(m_dict)
            self.diff.append(diff)

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        scalar = deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(deps), 2):
            frac, exp = deps[i:i+2]
            frac_id = frac.id
            exp_data = exp.data
            m_dict[frac_id] = exp_data
        op = create_monomial_op(m_dict)
        return op

def validate_monomial_dict(m_dict: Dict[int,"Float"]) -> None:
    assert isinstance(m_dict, dict) and -1 in m_dict, m_dict
    for op_id, exp_data in m_dict.items():
        assert isinstance(op_id, int), type(op_id)
        assert isinstance(exp_data, FloatTypes), type(exp_data)
        if op_id == -1:
            continue
        assert isinstance(exp_data, Fraction), type(exp_data)
        frac = OpDef.get_op(op_id)

def get_monomial_dict(op: "Op") -> Dict[int,"Float"]:
    if isinstance(op, Monomial):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            frac, exp = op.deps[i:i+2]
            exp_data = exp.data
            frac_id = frac.id
            m_dict[frac_id] = exp_data
    elif isinstance(op, Scalar):
        scalar_data = op.data
        m_dict = {-1: scalar_data}
    else:
        op_id = op.id
        m_dict = {-1: One, op_id: One}
    validate_monomial_dict(m_dict)
    return m_dict

def merge_monomial_dict(
    m_dict1: Dict[int,"Float"],
    m_dict2: Dict[int,"Float"]) -> Dict[int,"Float"]:
    validate_monomial_dict(m_dict1)
    validate_monomial_dict(m_dict2)
    m_dict = m_dict2.copy()
    for op_id, scalar_data in m_dict1.items():
        if op_id == -1:
            m_dict[-1] *= scalar_data
            if m_dict[-1] == Zero:
                m_dict = {-1: Zero}
                break
            continue
        if op_id not in m_dict:
            m_dict[op_id] = scalar_data
            continue
        scalar_data2 = m_dict[op_id]
        _sum = scalar_data2 + scalar_data
        if _sum == Zero:
            del m_dict[op_id]
        else:
            m_dict[op_id] = _sum
    validate_monomial_dict(m_dict)
    return m_dict

def create_monomial_op(m_dict: Dict[int,"Float"]) -> "Op":
    validate_monomial_dict(m_dict)
    scalar_data = m_dict[-1]
    if len(m_dict) == 1:
        op = OpDef.scalar(scalar_data)
        return op
    deps = []
    for op_id, exp_data in m_dict.items():
        if op_id == -1:
            continue
        nume = exp_data.numerator
        if nume == 0:
            continue
        frac = OpDef.get_op(op_id)
        if isinstance(frac, Scalar):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            inc = frac_data ** exp_data
            scalar_data *= inc
            continue
        deps.append(frac)
        exp = OpDef.scalar(exp_data)
        deps.append(exp)
    if scalar_data == Zero:
        op = OpDef.scalar(Zero)
        return op
    if len(deps) == 2 and scalar_data == One:
        exp = deps[1]
        exp_data = exp.data
        deno, nume = exp_data.denominator, exp_data.numerator
        if deno == 1 and nume == 1:
            op = deps[0]
            return op
    scalar = OpDef.scalar(scalar_data)
    deps = [scalar] + deps
    op = OpDef.monomial(*deps)
    return op

def validate_polynomial_dict(m_dict: Dict[int,"Float"]) -> None:
    assert isinstance(m_dict, dict) and -1 in m_dict, m_dict
    for op_id, coef_data in m_dict.items():
        assert isinstance(op_id, int), type(op_id)
        assert isinstance(coef_data, FloatTypes), type(coef_data)
        if op_id == -1:
            continue
        frac = OpDef.get_op(op_id)

def get_polynomial_dict(op: "Op") -> Dict[int,"Float"]:
    if isinstance(op, Polynomial):
        scalar = op.deps[0]
        scalar_data = scalar.data
        m_dict = {-1: scalar_data}
        for i in range(1, len(op.deps), 2):
            var, coef = op.deps[i:i+2]
            coef_data = coef.data
            var_id = var.id
            m_dict[var_id] = coef_data
    elif isinstance(op, Scalar):
        scalar_data = op.data
        m_dict = {-1: scalar_data}
    else:
        op_id = op.id
        m_dict = {-1: Zero, op_id: One}
    validate_polynomial_dict(m_dict)
    return m_dict

def merge_polynomial_dict(
    m_dict1: Dict[int,"Float"],
    m_dict2: Dict[int,"Float"]) -> Dict[int,"Float"]:
    validate_polynomial_dict(m_dict1)
    validate_polynomial_dict(m_dict2)
    m_dict = m_dict2.copy()
    for op_id, scalar_data in m_dict1.items():
        if op_id == -1:
            m_dict[-1] += scalar_data
            continue
        if op_id not in m_dict:
            m_dict[op_id] = scalar_data
            continue
        scalar_data2 = m_dict[op_id]
        _sum = scalar_data2 + scalar_data
        if _sum == Zero:
            del m_dict[op_id]
        else:
            m_dict[op_id] = _sum
    validate_polynomial_dict(m_dict)
    return m_dict

def create_polynomial_op(m_dict: Dict[int,"Float"]) -> "Op":
    validate_polynomial_dict(m_dict)
    scalar_data = m_dict[-1]
    if len(m_dict) == 1:
        op = OpDef.scalar(scalar_data)
        return op
    deps = []
    for op_id, coef_data in m_dict.items():
        if op_id == -1:
            continue
        if coef_data == Zero:
            continue
        var = OpDef.get_op(op_id)
        if isinstance(var, Scalar):
            var_data = var.data
            inc = var_data * coef_data
            scalar_data += inc
            continue
        deps.append(var)
        coef = OpDef.scalar(coef_data)
        deps.append(coef)
    if len(deps) == 2 and scalar_data == Zero:
        coef = deps[1]
        coef_data = coef.data
        if coef_data == One:
            op = deps[0]
            return op
    scalar = OpDef.scalar(scalar_data)
    deps = [scalar] + deps
    op = OpDef.polynomial(*deps)
    return op


@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
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
@register_opt("topo_degenerate")
@register_opt("topo_standardize")
@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
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
@register_opt("topo_degenerate")
@register_opt("topo_fuse")
@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
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
@register_op(
    valid_func=num_valid_func(1),
    equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func: FwdFuncType = lambda v: np.cos(v)


@register_opt("topo_standardize")
@register_op(
    valid_func=num_valid_func(2),
    equiv_func=swappable_equiv_func)
class Add(Op):
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
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x0, x1 = self.deps
        d0, d1 = x0.diff, x1.diff
        self.diff.clear()
        for i in range(len(var_seq)):
            dop = OpDef.add(d0[i], d1[i])
            self.diff.append(dop)


@register_op(
    valid_func=num_valid_func(2),
    equiv_func=sequential_equiv_func)
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
@register_op(
    valid_func=num_valid_func(2),
    equiv_func=swappable_equiv_func)
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
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        x, y = deps
        if isinstance(x, Scalar) and x.data == Zero or \
            isinstance(y, Scalar) and y.data == Zero:
            return OpDef.scalar(0)
        if isinstance(x, Scalar) and x.data == One:
            return y
        if isinstance(y, Scalar) and y.data == One:
            return x
        return super().topo_degenerate(*deps)

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
    deno, nume = exp_data.denominator, exp_data.numerator
    if frac_data == Zero:
        assert nume >= Zero, \
            "zero division occurs: frac_data: {}".format(
                frac_data) + ", exp_data: {}".format(exp_data)
    elif frac_data < Zero:
        assert deno == 1, \
            "the denominator of exp must be one for negative fraction," + \
            " frac_data: {}, exp_data: {}".format(frac_data, exp_data)


class ExpContradictError(Exception):
    pass


@register_opt("topo_standardize")
@register_op(
    valid_func=power_valid_func,
    equiv_func=sequential_equiv_func)
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
            "invalid exp_data: {}, run degenerate first".format(exp_data)
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
                        "dep_ids: {}".format([dep.id for dep in deps]))
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
            assert isinstance(data, Fraction), type(data)
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
                    assert isinstance(m_dict[nid], Fraction), type(m_dict[nid])
        # update m_dict by power
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                scalar_data = data ** exp_data
                m_dict[-1] = scalar_data
                continue
            ndata = data * exp_data
            assert isinstance(ndata, Fraction), type(ndata)
            m_dict[op_id] = ndata
        # m_dict: {-1: scalar_data, i1: e1, i2: e2, ... }
        # in case that en = Fraction(2*k, deno_in)
        # and op with op_id i_n is abs(sop)
        # then op should be turned into sop
        nm_dict = m_dict.copy()
        for op_id, data in nm_dict.items():
            if op_id == -1:
                continue
            assert isinstance(data, Fraction), type(data)
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
                    assert isinstance(m_dict[sid], Fraction), type(m_dict[sid])
        # create monomial op
        op = create_monomial_op(m_dict)
        return op

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> None:
        frac, exp = deps
        exp_data = exp.data
        assert isinstance(exp_data, Fraction), type(exp_data)
        nume = exp_data.numerator
        if nume == 0:
            op = OpDef.scalar(One)
            return op
        if isinstance(frac, Scalar):
            frac_data = frac.data
            validate_exp(frac_data, exp_data)
            scalar_data = frac_data ** exp_data
            op = OpDef.scalar(scalar_data)
            return op
        deno = exp_data.denominator
        if deno == 1 and nume == 1:
            return frac
        return cls.default_op(*deps)

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


@register_op(
    valid_func=num_valid_func(2),
    equiv_func=sequential_equiv_func)
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


@register_opt("topo_standardize")
@register_opt("topo_fuse")
@register_op(
    valid_func=num_valid_func(4),
    equiv_func=sequential_equiv_func)
class LessThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    def infer_sign(self) -> "OpSign":
        a_sign, b_sign, x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        if infer_lessthan_sign(a_sign, b_sign):
            return x_sign
        if infer_nomorethan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        od_func = getattr(OpDef, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)


@register_opt("topo_standardize")
@register_opt("topo_fuse")
@register_op(
    valid_func=num_valid_func(4),
    equiv_func=sequential_equiv_func)
class NoMoreThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

    def infer_sign(self) -> "OpSign":
        a_sign, b_sign, x_sign, y_sign = [dep.get_sign() for dep in self.deps]
        if infer_nomorethan_sign(a_sign, b_sign):
            return x_sign
        if infer_lessthan_sign(b_sign, a_sign):
            return y_sign
        sign = infer_mutual_sign(x_sign, y_sign)
        return sign

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        lhs, rhs, lv, rv = deps
        if lv.id == rv.id:
            # TODO: unittest
            return lv
        return super().topo_degenerate(*deps)

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

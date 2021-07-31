from typing import Dict, List, Callable, Optional, Union, Any, Set
import logging
from fractions import Fraction

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
    lambda op_type, ops: list(set([
        "{}:[{}]".format(op_type, ",".join(
            [str(ops[0].token()), str(ops[1].token())])),
        "{}:[{}]".format(op_type, ",".join(
            [str(ops[1].token()), str(ops[0].token())])),
    ]))

# Zero = np.float64(0.0)
# One = np.float64(1.0)
Zero = Fraction(0)
One = Fraction(1)

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

    def token(self):
        return self.id

    def forward(self) -> None:
        vs: List["np.float64"] = \
            [np.float64(dep.data) for dep in self.deps]
        self.data = self.__class__.fwd_func(*vs)

    @classmethod
    def default_op(cls, *deps: "Op") -> "Op":
        od_func = getattr(OpDef, cls.op_type)
        return od_func(*deps)

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    # @classmethod
    # def topo_fusepower(cls, *deps: "Op") -> "Op":
        # return cls.default_op(*deps)

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
    def topo_validate(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    @classmethod
    def dfs_fusepower(cls, *deps: "Op") -> "Op":
        pass

    def set_id(self, op_id: int) -> None:
        self.id = op_id

    def set_data(self, data: "Float") -> None:
        self.data = data

    def backward(self, grad: "Float"=cast_fraction(1)) -> None:
        grad = cast_fraction(grad)
        self.grad += grad
        for i, dep in enumerate(self.deps):
            backward_grad = self._grad_fns[i](grad)
            dep.backward(backward_grad)

    def reset(self) -> None:
        self.data = cast_fraction(0)
        self.diff.clear()
        self.sym = None

    def info(self, with_data=True) -> str:
        deps_info = ""
        if self.deps:
            deps_info = "deps:" + \
                ",".join([str(dep.id) for dep in self.deps])
        data_info = ""
        if with_data:
            data_info = "data:{}".format(self.data)
        s = "id:{},op_type:{}".format(self.id, self.op_type)
        return ",".join([s, data_info, deps_info])

    def display(self, logger=logging.getLogger("op_info")) -> None:
        logger.info(self.info())

    def to_sym(self) -> None:
        name = self.info(with_data=False)
        dep_syms = [dep.sym for dep in self.deps]
        if not dep_syms:
            self.sym = mx.sym.var(name=name)
        else:
            self.sym = mx.sym.add_n(*dep_syms, name=name)

    @classmethod
    def validate(cls, *deps: "Op") -> None:
        pass

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        raise NotImplementedError

    def autograph_forward(self) -> "Op":
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


@register_op(0)
class Scalar(Op):
    is_scalar: bool = True

    def forward(self):
        pass

    def reset(self) -> None:
        self.diff.clear()
        self.sym = None

    def to_sym(self) -> None:
        name = self.info()
        self.sym = mx.sym.var(name=name)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [None] * len(var_seq)


@register_opt("topo_validate")
@register_opt("topo_fuse")
@register_opt("topo_standardize")
@register_opt("topo_degenerate")
@register_op(0)
class Var(Op):
    @classmethod
    def topo_toscalar(cls, *deps: "Op") -> "Op":
        return cls.default_op(*deps)

    def forward(self):
        pass

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        self.diff = [None] * len(var_seq)
        self.diff[var_seq[self.id]] = OpDef.scalar(1.0)


@register_op(None, equiv_func=sequential_equiv_func)
class Polynomial(Op):
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
    @classmethod
    def fwd_func(cls, *v: "Float") -> "Float":
        product = v[0]
        for i in range(1, len(v), 2):
            if v[i+1] == One:
                product *= v[i]
            else:
                product *= v[i]**v[i+1]
        return product

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


@register_op(1, equiv_func=sequential_equiv_func)
class Negative(Op):
    fwd_func: FwdFuncType = lambda v: -v

    @classmethod
    def topo_standardize(cls, deps: "Op") -> "Op":
        minus_one = OpDef.scalar(-1)
        x = deps
        op = OpDef.multiply(x, minus_one)
        return op


@register_opt("topo_fuse")
@register_opt("topo_validate")
@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_opt("topo_degenerate")
@register_op(1, equiv_func=sequential_equiv_func)
class Sin(Op):
    fwd_func: FwdFuncType = lambda v: np.sin(v)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x = self.deps[0]
        y = OpDef.cos(x)
        self.diff.clear()
        for di in self.deps[0].diff:
            if di is None:
                op = None
            else:
                op = OpDef.multiply(y, di)
            self.diff.append(op)


@register_opt("topo_fuse")
@register_op(1, equiv_func=sequential_equiv_func)
class Cos(Op):
    fwd_func: FwdFuncType = lambda v: np.cos(v)


@register_op(2, equiv_func=swappable_equiv_func)
class AssertNotEqual(Op):
    def forward(self) -> None:
        v0 = self.deps[0].data
        v1 = self.deps[1].data
        assert v0 != v1


@register_op(2, equiv_func=sequential_equiv_func)
class AssertLessThan(Op):
    def forward(self) -> None:
        v0 = self.deps[0].data
        v1 = self.deps[1].data
        assert v0 < v1


@register_op(2, equiv_func=sequential_equiv_func)
class AssertNoMoreThan(Op):
    def forward(self) -> None:
        v0 = self.deps[0].data
        v1 = self.deps[1].data
        assert v0 <= v1


@register_opt("topo_validate")
@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_op(2, equiv_func=swappable_equiv_func)
class Add(Op):
    _grad_fns: List["GradFuncType"] = [
        lambda grad: grad,
        lambda grad: grad,
    ]
    fwd_func: FwdFuncType = lambda v0, v1: v0 + v1

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
            if d0[i] is None:
                if d1[i] is None:
                    op = None
                else:
                    op = d1[i]
            else:
                if d1[i] is None:
                    op = d0[i]
                else:
                    op = OpDef.add(d0[i], d1[i])
            self.diff.append(op)


@register_op(2, equiv_func=sequential_equiv_func)
class Subtract(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 - v1

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> "Op":
        x, y = deps
        minus_one = OpDef.scalar(-1)
        minus_y = OpDef.multiply(minus_one, y)
        op = OpDef.add(x, minus_y)
        return op


@register_opt("topo_validate")
@register_opt("topo_standardize")
@register_op(2, equiv_func=swappable_equiv_func)
class Multiply(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 * v1

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

    # @classmethod
    # def topo_fusepower(cls, *deps: "Op") -> "Op":
        # x, y = deps
        # if isinstance(x, Power):
            # xx, xy = x.deps
            # if isinstance(y, Power):
                # yx, yy = y.deps
                # if xx.id == yx.id:
                    # nscalar = OpDef.scalar(xy.data+yy.data)
                    # op = OpDef.power(xx, nscalar)
                    # return op
            # elif isinstance(y, Var):
                # if xx.id == y.id:
                    # nscalar = OpDef.scalar(xy.data+1)
                    # op = OpDef.power(xx, nscalar)
                    # return op
        # elif isinstance(x, Var):
            # if isinstance(y, Power):
                # yx, yy = y.deps
                # if x.id == yx.id:
                    # nscalar = OpDef.scalar(1+yy.data)
                    # op = OpDef.power(x, nscalar)
                    # return op
            # if isinstance(y, Var):
                # if x.id == y.id:
                    # nscalar = OpDef.scalar(2)
                    # op = OpDef.power(x, nscalar)
                    # return op
        # return cls.default_op(*deps)

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
            if d0[i] is None:
                if d1[i] is None:
                    op = None
                else:
                    op = OpDef.multiply(x0, d1[i])
            else:
                op1 = OpDef.multiply(x1, d0[i])
                if d1[i] is None:
                    op = op1
                else:
                    op2 = OpDef.multiply(x0, d1[i])
                    op = OpDef.add(op1, op2)
            self.diff.append(op)

def validate_exp(frac_data: "Float", exp_data: "Fraction") -> None:
    if frac_data == Zero:
        assert exp_data >= Zero, \
            "zero division occurs: frac_data: {}".format(
                frac_data) + ", exp_data: {}".format(exp_data)
    elif frac_data < Zero:
        assert exp_data.denominator == 1, \
            "exp must be an integer for negative fraction, " + \
            "frac_data: {}, exp_data: {}".format(frac_data, exp_data)


@register_opt("topo_standardize")
@register_opt("topo_validate")
@register_op(2, equiv_func=sequential_equiv_func)
class Power(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0**v1

    @classmethod
    def topo_fuse(cls, *deps: "Op") -> "Op":
        x, y = deps
        exp_data = y.data
        if exp_data == Zero:
            op = OpDef.scalar(1)
            return op
        assert isinstance(exp_data, Fraction), exp_data
        m_dict = get_monomial_dict(x)
        frac_data = m_dict[-1]
        validate_exp(frac_data, exp_data)
        scalar_data = frac_data ** exp_data
        scalar = OpDef.scalar(scalar_data)
        if len(m_dict) == 1:
            return scalar
        if exp_data.denominator != 1:
            return super().topo_fuse(*deps)
        ndeps = [scalar]
        for op_id, scalar_data in m_dict.items():
            if op_id == -1:
                continue
            assert isinstance(scalar_data, Fraction)
            dep = OpDef.get_op(op_id)
            ndeps.append(dep)
            nexp_data = Fraction(scalar_data*exp_data)
            nexp = OpDef.scalar(nexp_data)
            ndeps.append(nexp)
        if len(ndeps) == 3 and ndeps[0].data == One and \
            ndeps[2].data == One:
            return ndeps[1]
        op = OpDef.monomial(*ndeps)
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

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        x, y = self.deps
        d = x.diff
        assert any([dd is not None for dd in d])
        self.diff.clear()
        nscalar = OpDef.scalar(y.data-1)
        npower = OpDef.power(x, nscalar)
        mul_scalar = OpDef.multiply(y, npower)
        for i in range(len(d)):
            if d[i] is None:
                op = None
            else:
                op = OpDef.multiply(mul_scalar, d[i])
            self.diff.append(op)

    @classmethod
    def validate(cls, *deps: "Op") -> None:
        x, y = deps
        assert isinstance(y, Scalar), \
            "type of deps[1]: {} must be scalar".format(
                type(y).__class__.__name__)


@register_op(2, equiv_func=sequential_equiv_func)
class Divide(Op):
    fwd_func: FwdFuncType = lambda v0, v1: v0 / v1

    @classmethod
    def topo_standardize(cls, *deps: "Op") -> None:
        x, y = deps
        minus_one = OpDef.scalar(-1)
        _pow = OpDef.power(y, minus_one)
        op = OpDef.multiply(x, _pow)
        return op

    # TODO: to deprecate
    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        raise RuntimeError(
            "Divide Op is not supported in autograph_backward")
        # x0, x1 = self.deps
        # d0, d1 = x0.diff, x1.diff
        # self.diff.clear()
        # for i in range(len(d0)):
            # if d0[i] is None:
                # if d1[i] is None:
                    # op = None
                # else:
                    # op1 = OpDef.multiply(self, d1[i])
                    # op2 = OpDef.divide(op1, x1)
                    # op = OpDef.negative(op2)
            # else:
                # if d1[i] is None:
                    # op = OpDef.divide(d0[i], x1)
                # else:
                    # op1 = OpDef.multiply(self, d1[i])
                    # op2 = OpDef.subtract(d0[i], op1)
                    # op = OpDef.divide(op2, x1)
            # self.diff.append(op)

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
@register_opt("topo_validate")
@register_op(4, equiv_func=default_equiv_func)
class LessThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 < v1 else v3

    @classmethod
    def topo_degenerate(cls, *deps: "Op") -> "Op":
        return cnd_topo_degenerate(cls.default_op, *deps)

    def autograph_backward(self, var_seq: Dict[int,int]) -> None:
        od_func = getattr(OpDef, self.op_type)
        self.diff = cnd_auto_backward(self.deps, od_func, var_seq)


@register_opt("topo_standardize")
@register_opt("topo_toscalar")
@register_opt("topo_validate")
@register_op(4, equiv_func=default_equiv_func)
class NoMoreThan(Op):
    fwd_func: FwdFuncType = lambda v0, v1, v2, v3: v2 if v0 <= v1 else v3

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
        OpDef.scalar_map[nv] = op
        OpDef.id_map[op.id] = op
        return op

    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            equivs: List[str] = op_cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in OpDef.equiv_map:
                    equiv_op: "Op" = OpDef.equiv_map[equiv]
                    return equiv_op
            op_cls.validate(*deps)
            op: "Op" = op_cls(*deps)
            OpDef.set_id(op)
            for equiv in equivs:
                OpDef.equiv_map[equiv] = op
            OpDef.id_map[op.id] = op
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

    @staticmethod
    def reset():
        OpDef.current_id = 0
        OpDef.equiv_map.clear()
        OpDef.id_map.clear()
        OpDef.scalar_map.clear()

    @staticmethod
    def set_id(op: "Op") -> None:
        op.set_id(OpDef.current_id)
        OpDef.current_id += 1

    @staticmethod
    def get_op(op_id: int) -> None:
        assert op_id in OpDef.id_map, \
            "invalid op_id: {}".format(op_id)
        return OpDef.id_map[op_id]

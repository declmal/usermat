class Operator(object):
    def __init__(self, *childs, *params):
        self._validate_childs(*childs)
        self._validate_params(*params)
        self.childs = childs
        self.params = params

    def forward(self, *args):
        self._validate_args(*args)
        return self._impl(*args)

    def _impl(self, *args):
        return self._op(*args)

    def _op(self, *args):
        raise NotImplementedError

    def _validate_childs(self, *childs):
        assert len(childs) == 1, \
            "invalid number of childs: {}".format(childs)
        assert isinstance(childs[0], Operator), \
            "invalid type of child: {}, self: {}".format(childs[0], self)

    def _validate_params(self, *params):
        assert len(params) == 0, \
            "invalid number of params: {}".format(params)

    def _validate_args(self, *args):
        raise NotImplementedError


class Unary(Operator):
    def _validate_args(self, *args):
        assert len(args) == 1, \
            "invalid number of args: {}".format(args)


class ParamOp(Operator):
    def _validate_params(self, *params):
        assert len(params) == 1, \
            "invalid number of params: {}".format(params)


class Logical(Operator):
    def _validate_childs(self, *childs):
        assert len(childs) == 2, \
            "invalid number of childs: {}".format(childs)
        for child in childs:
            assert isinstance(child, Operator), \
                "invalid type of child: {}, self: {}".format(child, self)


class LogicalScalar(Unary, ParamOp, Logical):
    def _impl(self, *args):
        return self.childs[0].forward(args[0]) if self._op(*args) \
            else self.childs[1].forward(args[1])


class LeqScalar(LogicalScalar):
    def _op(self, *args):
        return args[0] <= self.params[0]


class MulScalar(Unary, ParamOp):
    def _op(self, *args):
        return self.params[0] * args[0]


class Output(Unary):
    def _op(self, *args):
        return args[0]


class Scalar(self):
    pass

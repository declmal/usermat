from .expr_utils import validate_strings
from ..expr_def import ExprDef as ed


@ed.register_expr
class Binary(ed.get_expr_cls("stringlist")):
    def __init__(self, lhs, rhs):
        assert isinstance(lhs, list), \
            "invalid type: {} of lhs: {}".format(type(lhs), lhs)
        validate_strings(*lhs)
        assert isinstance(rhs, list), \
            "invalid type: {} of rhs: {}".format(type(rhs), rhs)
        validate_strings(*rhs)
        self.strings = []
        for string in lhs:
            self.strings.append(string)
        op = self.get_op()
        self.strings.append(op)
        for string in rhs:
            self.strings.append(string)


@ed.register_expr
class Assignment(ed.get_expr_cls("binary")):
    def get_op(self):
        return " = "

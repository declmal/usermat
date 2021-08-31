from .expr_utils import (
    codegen_line, supported_data_types, validate_strings,
    validate_unique_strings
)
from ..expr_def import ExprDef as ed


class Expr(object):
    def codegen(self):
        raise NotImplementedError


@ed.register_expr
class StringList(Expr):
    def __init__(self, *strings):
        self.strings = []
        for string in strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            self.strings.append(string)

    def codegen(self):
        code_lines = []
        for string in self.strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            code_lines = codegen_line(string, code_lines=code_lines)
        code = "".join(code_lines)
        return code


@ed.register_expr
class CommaList(StringList):
    def __init__(self, *strings):
        self.strings = []
        for string in strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            if self.strings:
                self.strings.append(", ")
            self.strings.append(string)


@ed.register_expr
class Tuple(CommaList):
    def __init__(self, *strings):
        super().__init__(*strings)
        self.strings = ["("] + self.strings + [")"]


@ed.register_expr
class Declaration(CommaList):
    def __init__(self, *variables, data_type="real*8"):
        assert isinstance(data_type, str) and \
            data_type in supported_data_types, \
            "invalid data type: {}".format(data_type)
        validate_unique_strings(*variables)
        super().__init__(*variables)
        self.strings = [data_type, " :: "] + self.strings


@ed.register_expr
class Condition(Expr):
    pass


@ed.register_expr
class Subroutine(Expr):
    def __init__(self, func_name, arg_tuple, printable_list):
        self.func_name = func_name
        self.strings = [
            String("subroutine "),
            String(func_name),
            arg_tuple
        ]
        for printable in printable_list:
            self.strings.append(printable)
        self.strings.append(String("return"))
        self.strings.append(String("end"))

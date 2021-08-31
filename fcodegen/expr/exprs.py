from .expr_utils import (
    codegen_line, supported_data_types, validate_strings,
    validate_unique_strings, default_start_col
)
from ..expr_def import ExprDef as ed


class Expr(object):
    def codegen(self):
        raise NotImplementedError


@ed.register_expr
class CodeBlock(Expr):
    def __init__(self, *strings, start_col=default_start_col):
        self.strings = []
        for string in strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            self.strings.append(string)
        self.start_col = start_col

    def codegen(self):
        code_lines = []
        for string in self.strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            code_lines = codegen_line(
                string, code_lines=code_lines, start_col=self.start_col)
        code = "".join(code_lines)
        return code


@ed.register_expr
class Assignment(ed.get_expr_cls("codeblock")):
    def __init__(self, lhs, *rhs, start_col=default_start_col):
        assert isinstance(lhs, str), \
            "invalid type: {} of lhs: {}".format(type(lhs), lhs)
        validate_strings(*rhs)
        self.strings = [lhs, " = "] + list(rhs)
        self.start_col = start_col


@ed.register_expr
class CommaList(CodeBlock):
    def __init__(self, *strings):
        validate_strings(*strings)
        self.strings = []
        for string in strings:
            if self.strings:
                self.strings.append(", ")
            self.strings.append(string)


@ed.register_expr
class Tuple(CommaList):
    def __init__(self, *strings, start_col=default_start_col):
        super().__init__(*strings)
        self.strings = ["("] + self.strings + [")"]
        self.start_col = start_col


@ed.register_expr
class Declaration(CodeBlock):
    def __init__(
        self, *variables, data_type="real*8", start_col=default_start_col):
        assert isinstance(data_type, str) and \
            data_type in supported_data_types, \
            "invalid data type: {}".format(data_type)
        validate_unique_strings(*variables)
        strings = []
        for string in variables:
            if strings:
                strings.append(", ")
            strings.append(string)
        self.strings = [data_type, " :: "] + strings
        self.start_col = default_start_col


@ed.register_expr
class EndIfStmt(CodeBlock):
    def __init__(self, start_col=default_start_col):
        self.strings = ["endif"]
        self.start_col = start_col


@ed.register_expr
class IfStmt(CodeBlock):
    def __init__(
        self, *logicals, start_col=default_start_col, with_else=False):
        validate_strings(*logicals)
        stmt = "elseif " if with_else else "if "
        self.strings = [stmt, "("] + list(logicals)+ [")", " then"]
        self.start_col = start_col


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

    def codegen(self):
        pass

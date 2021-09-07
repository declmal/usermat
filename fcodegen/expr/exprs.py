from .expr_utils import (
    codegen_line, supported_data_types, validate_strings,
    validate_unique_strings, default_start_col, default_inc_col
)
from ..expr_def import ExprDef as ed


@ed.register_expr
class CodeBlock:
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

    def inc_start_col(self):
        return self.start_col + default_inc_col


@ed.register_expr
class Assignment(ed.get_expr_cls("codeblock")):
    def __init__(self, lhs, *rhs, start_col=default_start_col):
        assert isinstance(lhs, str), \
            "invalid type: {} of lhs: {}".format(type(lhs), lhs)
        validate_strings(*rhs)
        self.strings = [lhs, " = "] + list(rhs)
        self.start_col = start_col


@ed.register_expr
class CommaList(ed.get_expr_cls("codeblock")):
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
class Declaration(ed.get_expr_cls("codeblock")):
    def __init__(
        self, *variables, data_type="real*8",
        start_col=default_start_col):
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
class IfStmt(ed.get_expr_cls("codeblock")):
    def __init__(
        self, *logicals, start_col=default_start_col, with_else=False):
        validate_strings(*logicals)
        stmt = "elseif " if with_else else "if "
        self.strings = [stmt, "("] + list(logicals)+ [")", " then"]
        self.start_col = start_col


@ed.register_expr
class ElseStmt(ed.get_expr_cls("codeblock")):
    def __init__(self, start_col=default_start_col):
        self.strings = ["else"]
        self.start_col = start_col


@ed.register_expr
class EndIfStmt(ed.get_expr_cls("codeblock")):
    def __init__(self, start_col=default_start_col):
        self.strings = ["endif"]
        self.start_col = start_col


@ed.register_expr
class ReturnStmt(ed.get_expr_cls("codeblock")):
    def __init__(self, start_col=default_start_col):
        self.strings = ["return"]
        self.start_col = start_col


@ed.register_expr
class EndStmt(ed.get_expr_cls("codeblock")):
    def __init__(self, start_col=default_start_col):
        self.strings = ["end"]
        self.start_col = start_col


@ed.register_expr
class Formula(ed.get_expr_cls("codeblock")):
    def __init__(self, form_name, arguments, codeblocks):
        strings = [form_name] + arguments
        validate_unique_strings(*strings)
        for i, codeblock in enumerate(codeblocks):
            if i == 0:
                expr_type = ed.get_expr_cls("declaration")
            else:
                expr_type = ed.get_expr_cls("assignment")
            assert isinstance(codeblock, expr_type), \
                "invalid type: {} of codeblock: {}, expected: {}".format(
                    type(codeblock), codeblock, expr_type)
        strings = ["subroutine ", form_name, "("]
        strings.append(")")
        for i, argument in enumerate(arguments):
            strings.append(argument)
            if i < len(arguments) - 1:
                strings.append(", ")
        headline = ed.codeblock(*strings)
        self.codeblocks = [headline]
        for codeblock in codeblocks:
            self.codeblocks.append(codeblock)
        returnstmt = ed.returnstmt()
        self.codeblocks.append(returnstmt)
        endstmt = ed.endstmt()
        self.codeblocks.append(endstmt)

    def codegen(self):
        codes = []
        for codeblock in self.codeblocks:
            code = codeblock.codegen()
            codes.append(code)
        ret = "\n".join(codes)
        return ret

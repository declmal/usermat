class CodePrinter(object):
    pass


start_col = 7
maximum_col = 73
default_prefix = " " * 6


class Printable(object):
    def codegen(self):
        raise NotImplementedError


class MaxColumnExceedError(Exception):
    pass


def codegen_line(string, code_lines):
    assert isinstance(string, str), \
        "invalid type of string: {}".format(string)
    if not code_lines:
        code_lines.append(default_prefix)
    cur_line = code_lines[-1]
    cur_len = len(cur_line)
    string_len = len(string)
    end_col = cur_len + string_len
    if end_col <= maximum_col:
        cur_line += string
        code_lines[-1] = cur_line
        return code_lines
    cur_line += "\n"
    code_lines[-1] = cur_line
    cur_line = " "*(start_col-2) + "&"
    end_col = start_col - 1 + string_len
    if end_col > maximum_col:
        error_info = "string: {} exceed maximum col: {}".format(
            string, maximum_col)
        raise MaxColumnExceedError(error_info)
    cur_line += string
    code_lines.append(cur_line)
    return code_lines


class StringList(Printable):
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


class CommaList(StringList):
    def __init__(self, *strings):
        self.strings = []
        for string in strings:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            if self.strings:
                self.strings.append(", ")
            self.strings.append(string)


class TupleCommaList(CommaList):
    def __init__(self, *strings):
        super().__init__(*strings)
        self.strings = ["("] + self.strings + [")"]

supported_data_types = ["real*8", "real"]

def validate_unique(*strings):
    string_set = set()
    for string in strings:
        assert isinstance(string, str), \
            "invalid type of string instance: {}".format(type(string))
        assert string not in string_set, \
            "duplicate string: {} in strings: {}".format(string, strings)
        string_set.add(string)


class Declaration(CommaList):
    def __init__(self, *variables, data_type="real*8"):
        assert isinstance(data_type, str) and \
            data_type in supported_data_types, \
            "invalid data type: {}".format(data_type)
        validate_unique(*variables)
        super().__init__(*variables)
        self.strings = [data_type, " :: "] + self.strings


class Assignment(StringList):
    def __init__(self, lhs, *rhs):
        assert isinstance(lhs, str), \
            "invalid type of lhs: {}".format(type(lhs))
        self.strings = [lhs, " = "]
        for string in rhs:
            assert isinstance(string, str), \
                "invalid type of string instance: {}".format(type(string))
            self.strings.append(string)


class Subroutine:
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

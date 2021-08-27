class CodePrinter(object):
    pass


start_col = 7
maximum_col = 73
default_prefix = " " * 6


class Printable(object):
    def codegen(self, prefix=default_prefix):
        raise NotImplementedError


class MaxColumnExceedError(Exception):
    pass


class String(Printable):
    def __init__(self, s):
        self.s = s

    def codegen(self, prefix=default_prefix):
        string_len = len(self.s)
        prefix_len = len(prefix)
        end_col = prefix_len + string_len
        if end_col <= maximum_col:
            ret = prefix + self.s
            return ret
        prefix = " " * (start_col-2) + "&"
        end_col = prefix_len + string_len
        if end_col > maximum_col:
            error_info = "string: {} exceed maximum col: {}".format(
                self.s, maximum_col)
            raise MaxColumnExceedError(error_info)
        ret = prefix + self.s
        return ret


class Tuple:
    def __init__(self, printable):
        self.contents = [
            String("("),
            printable,
            String(")")
        ]


class StringList:
    def __init__(self, var_list):
        self.contents = []
        for var in var_list:
            if self.contents:
                self.contents.append(String(", "))
            self.contents.append(var)


class Declaration:
    def __init__(self, data_type, var_list):
        self.contents = [
            String(data_type),
            String(" :: "),
            var_list
        ]


class Assignment:
    def __init__(self, lhs, rhs):
        self.contents = [
            lhs,
            String(" = "),
            rhs
        ]


class Subroutine:
    def __init__(self, func_name, arg_tuple, printable_list):
        self.func_name = func_name
        self.contents = [
            String("subroutine "),
            String(func_name),
            arg_tuple
        ]
        for printable in printable_list:
            self.contents.append(printable)
        self.contents.append(String("return"))
        self.contents.append(String("end"))

default_start_col = 7
default_maximum_col = 73
default_inc_col = 2
supported_data_types = ["real*8", "real"]


class MaxColumnExceedError(Exception):
    pass


def codegen_line(
    string, code_lines, start_col=default_start_col,
    maximum_col=default_maximum_col):
    assert isinstance(string, str), \
        "invalid type of string: {}".format(string)
    if not code_lines:
        default_prefix = " " * (start_col-1)
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

def validate_strings(*strings):
    for string in strings:
        assert isinstance(string, str), \
            "invalid type: {} of string instance: {}".format(
                type(string), string)

def validate_unique_strings(*strings):
    validate_strings(*strings)
    string_set = set()
    for string in strings:
        assert string not in string_set, \
            "duplicate string: {} in strings: {}".format(string, strings)
        string_set.add(string)

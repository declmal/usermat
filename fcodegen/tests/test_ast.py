import unittest

from ..expr.expr_utils import codegen_line
from ..expr_def import ExprDef as ed
from .test_utils import register_test
from ..utils.type_utils import MinusOne, One, Half, Zero
from ..op_def import OpDef as od
from ..op_reg import OpReg as org


@register_test
class TestAst(unittest.TestCase):
    def test_codegen_line(self):
        string = "asd asads sdw asdw dfsfwer dfsqwe asda  adsadwe asda"
        code_lines = codegen_line(string, [])
        code = code_lines[0]
        code_ref = "      " + string
        self.assertEqual(code, code_ref)

    def test_tuple_string_list(self):
        string = "abcdefghi   "
        strings = [string] * 10
        strlst = ed.tuple(*strings)
        code = strlst.codegen()
        code_ref = \
            "      (abcdefghi   , abcdefghi   , abcdefghi   ," + \
            " abcdefghi   , \n" + \
            "     &abcdefghi   , abcdefghi   , abcdefghi   , " + \
            "abcdefghi   , \n" + \
            "     &abcdefghi   , abcdefghi   )"
        self.assertEqual(code, code_ref)

    def test_declaration(self):
        strings = ["x", "y", "z"]
        dec = ed.declaration(*strings, data_type="real")
        code = dec.codegen()
        code_ref = "      real :: x, y, z"
        self.assertEqual(code, code_ref)

    def test_sin(self):
        v0 = od.var("x")
        v1 = od.sin(v0)
        ast_func = org.get_opt(v1, "dfs_ast")
        assignments = []
        ast_func(v1, [], [], assignments)
        assignment = assignments[0]
        code = assignment.codegen()
        name = v1.name
        code_ref = "      {} = sin(x)".format(name)
        self.assertEqual(code, code_ref)

    def test_monomial(self):
        v0 = od.var("x")
        v1 = od.var("y")
        v2 = od.var("z")
        v3 = od.var("u")
        s0 = od.scalar(MinusOne)
        s1 = od.scalar(One)
        s2 = od.scalar(Half)
        s3 = od.scalar(-Half)
        op_list = [
            od.monomial(s0, v0, s0, v1, s1, v2, s2, v3, s3),
            od.monomial(s1, v0, s0, v1, s1, v2, s2, v3, s3),
            od.monomial(s2, v0, s0, v1, s1, v2, s2, v3, s3),
            od.monomial(s3, v0, s0, v1, s1, v2, s2, v3, s3),
            od.monomial(s0, v0, s1, v1, s0, v2, s2, v3, s3),
            od.monomial(s1, v0, s1, v1, s0, v2, s2, v3, s3),
            od.monomial(s2, v0, s1, v1, s0, v2, s2, v3, s3),
            od.monomial(s3, v0, s1, v1, s0, v2, s2, v3, s3),
            od.monomial(s0, v0, s2, v1, s1, v2, s0, v3, s3),
            od.monomial(s1, v0, s2, v1, s1, v2, s0, v3, s3),
            od.monomial(s2, v0, s2, v1, s1, v2, s0, v3, s3),
            od.monomial(s3, v0, s2, v1, s1, v2, s0, v3, s3),
            od.monomial(s0, v0, s3, v1, s1, v2, s2, v3, s0),
            od.monomial(s1, v0, s3, v1, s1, v2, s2, v3, s0),
            od.monomial(s2, v0, s3, v1, s1, v2, s2, v3, s0),
            od.monomial(s3, v0, s3, v1, s1, v2, s2, v3, s0),
        ]
        code_list = []
        for op in op_list:
            ast_func = org.get_opt(op, "dfs_ast")
            assignments = []
            ast_func(op, [], [], assignments)
            expr = assignments[0]
            code = expr.codegen()
            code_list.append(code)
        name_list = [op.name for op in op_list]
        code_ref_list = [
            "      {} = -1.0 / x * y * z ** 0.5 / u ** 0.5".format(
                name_list[0]),
            "      {} = 1.0 / x * y * z ** 0.5 / u ** 0.5".format(
                name_list[1]),
            "      {} = 0.5 / x * y * z ** 0.5 / u ** 0.5".format(
                name_list[2]),
            "      {} = -0.5 / x * y * z ** 0.5 / u ** 0.5".format(
                name_list[3]),
            "      {} = -x / y * z ** 0.5 / u ** 0.5".format(
                name_list[4]),
            "      {} = x / y * z ** 0.5 / u ** 0.5".format(
                name_list[5]),
            "      {} = 0.5 * x / y * z ** 0.5 / u ** 0.5".format(
                name_list[6]),
            "      {} = -0.5 * x / y * z ** 0.5 / u ** 0.5".format(
                name_list[7]),
            "      {} = -x ** 0.5 * y / z / u ** 0.5".format(
                name_list[8]),
            "      {} = x ** 0.5 * y / z / u ** 0.5".format(
                name_list[9]),
            "      {} = 0.5 * x ** 0.5 * y / z / u ** 0.5".format(
                name_list[10]),
            "      {} = -0.5 * x ** 0.5 * y / z / u ** 0.5".format(
                name_list[11]),
            "      {} = -1.0 / x ** 0.5 * y * z ** 0.5 / u".format(
                name_list[12]),
            "      {} = 1.0 / x ** 0.5 * y * z ** 0.5 / u".format(
                name_list[13]),
            "      {} = 0.5 / x ** 0.5 * y * z ** 0.5 / u".format(
                name_list[14]),
            "      {} = -0.5 / x ** 0.5 * y * z ** 0.5 / u".format(
                name_list[15]),
        ]
        for i in range(len(code_ref_list)):
            code = code_list[i]
            code_ref = code_ref_list[i]
            self.assertEqual(code, code_ref)

    def test_polynomial(self):
        v0 = od.var("x")
        v1 = od.var("y")
        v2 = od.var("z")
        v3 = od.var("u")
        s0 = od.scalar(MinusOne)
        s1 = od.scalar(One)
        s2 = od.scalar(Half)
        s3 = od.scalar(-Half)
        s4 = od.scalar(Zero)
        op_list = [
            od.polynomial(s2, v0, s0, v1, s1, v2, s2, v3, s3),
            od.polynomial(s3, v0, s0, v1, s1, v2, s2, v3, s3),
            od.polynomial(s4, v0, s0, v1, s1, v2, s2, v3, s3),
            od.polynomial(s2, v0, s1, v1, s0, v2, s2, v3, s3),
            od.polynomial(s3, v0, s1, v1, s0, v2, s2, v3, s3),
            od.polynomial(s4, v0, s1, v1, s0, v2, s2, v3, s3),
            od.polynomial(s2, v0, s2, v1, s1, v2, s0, v3, s3),
            od.polynomial(s3, v0, s2, v1, s1, v2, s0, v3, s3),
            od.polynomial(s4, v0, s2, v1, s1, v2, s0, v3, s3),
            od.polynomial(s2, v0, s3, v1, s1, v2, s2, v3, s0),
            od.polynomial(s3, v0, s3, v1, s1, v2, s2, v3, s0),
            od.polynomial(s4, v0, s3, v1, s1, v2, s2, v3, s0),
        ]
        code_list = []
        for op in op_list:
            ast_func = org.get_opt(op, "dfs_ast")
            assignments = []
            ast_func(op, [], [], assignments)
            expr = assignments[0]
            code = expr.codegen()
            code_list.append(code)
        name_list = [op.name for op in op_list]
        code_ref_list = [
            "      {} = 0.5 - x + y + z * 0.5 - u * 0.5".format(
                name_list[0]),
            "      {} = -0.5 - x + y + z * 0.5 - u * 0.5".format(
                name_list[1]),
            "      {} = -x + y + z * 0.5 - u * 0.5".format(
                name_list[2]),
            "      {} = 0.5 + x - y + z * 0.5 - u * 0.5".format(
                name_list[3]),
            "      {} = -0.5 + x - y + z * 0.5 - u * 0.5".format(
                name_list[4]),
            "      {} = x - y + z * 0.5 - u * 0.5".format(
                name_list[5]),
            "      {} = 0.5 + x * 0.5 + y - z - u * 0.5".format(
                name_list[6]),
            "      {} = -0.5 + x * 0.5 + y - z - u * 0.5".format(
                name_list[7]),
            "      {} = x * 0.5 + y - z - u * 0.5".format(
                name_list[8]),
            "      {} = 0.5 - x * 0.5 + y + z * 0.5 - u".format(
                name_list[9]),
            "      {} = -0.5 - x * 0.5 + y + z * 0.5 - u".format(
                name_list[10]),
            "      {} = -x * 0.5 + y + z * 0.5 - u".format(
                name_list[11]),
        ]
        for i in range(len(code_ref_list)):
            code = code_list[i]
            code_ref = code_ref_list[i]
            self.assertEqual(code, code_ref)

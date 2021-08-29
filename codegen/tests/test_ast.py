import unittest

from ..ast import codegen_line, TupleCommaList, Declaration
from .test_utils import register_test
from ..utils.type_utils import MinusOne, One, Half
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
        strlst = TupleCommaList(*strings)
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
        dec = Declaration(*strings, data_type="real")
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
            od.monomial(s3, v0, s0, v1, s1, v2, s2, v3, s3)
        ]
        code_list = []
        for op in op_list:
            ast_func = org.get_opt(op, "dfs_ast")
            assignments = []
            ast_func(v1, [], [], assignments)
            assignments = assignments[0]
            code = assignment.codegen()
            code_list.append(code)
        name_list = [op.name for op in op_list]
        code_ref_list = []
        for name in name_list:
            code_ref = "      {} = 1.0 / "

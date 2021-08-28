import unittest

from ..ast import codegen_line, TupleCommaList, Declaration
from .test_utils import register_test
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

    def test_assignment(self):
        v0 = od.var("x")
        v1 = od.sin(v0)
        codegen_func = org.get_opt(v1, "dfs_ast")
        assignments = []
        codegen_func(v1, [], [], assignments)
        assignment = assignments[0]
        code = assignment.codegen()
        name = v1.name
        code_ref = "      {} = sin(x)".format(name)
        self.assertEqual(code, code_ref)

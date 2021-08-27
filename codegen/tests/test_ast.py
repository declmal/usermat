import unittest

from ..ast import String
from .test_utils import register_test


@register_test
class TestAst(unittest.TestCase):
    def test_string(self):
        s = "asd asads sdw asdw dfsfwer dfsqwe asda  adsadwe asda"
        string = String(s)
        s_gen = string.codegen()
        s_ref = "      " + s
        self.assertEqual(s_gen, s_ref)


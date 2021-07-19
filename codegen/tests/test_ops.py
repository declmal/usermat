import unittest

from codegen.ops import Var, Add
from codegen.graph import Graph

class TestOps(unittest.TestCase):
    def test_add(self):
        with Graph() as g:
            a = g.var()
            b = g.var()
            c = g.add(a, b)
            d = g.add(b, c)
            c.backward(grad=1)
            self.assertEqual(b.grad, 1)
            pass

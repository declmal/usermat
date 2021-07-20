import unittest

from codegen.ops import OpDef as od
from codegen.graph import Graph

class TestOps(unittest.TestCase):
    def test_add(self):
        a = od.var()
        b = od.var()
        c = od.add(a, b)
        d = od.add(b, c)
        g = Graph([a,b], [c,d])
        g.set_input(1, 2)
        outs1 = g.get_output()
        self.assertEqual(outs1, [3,5])
        g.reset()
        g.set_input(3, -1)
        outs2 = g.get_output()
        self.assertEqual(outs2, [2,1])
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)
        g.get_info()

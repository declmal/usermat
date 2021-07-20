import unittest

import numpy as np

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

    def test_2(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.var()
        v4 = od.divide(v0, v1)
        v5 = od.add(v4, v2)
        v6 = od.sin(v5)
        v7 = od.multiply(v3, v6)
        g = Graph([v0,v1,v2,v3], [v7])
        g.set_input(1, 2, 3, 4)
        outs1 = g.get_output()
        self.assertEqual(outs1, [np.sin(1.0/2.0+3.0)*4.0])
        g.get_mx()

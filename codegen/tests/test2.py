import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.op_def import OpDef as od
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.test_utils import register_test


@register_test
class Test2(unittest.TestCase):
    def test_2(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.var()
        v4 = od.divide(v0, v1)
        v5 = od.add(v4, v2)
        v6 = od.sin(v5)
        v7 = od.multiply(v3, v3)
        v8 = od.multiply(v7, v3)
        v9 = od.multiply(v6, v8)
        g = Graph([v0,v1,v2,v3], [v9])
        g.tosym()
        g.optimize()
        g.tosym()
        a, b, c, d = [float(v) for v in [1, 2, 3, 4]]
        outs1 = g.forward(a, b, c, d)
        self.assertEqual(outs1, [np.sin(a/b+c)*d*d*d])
        ng = g.autograph_backward()
        ng.fuse()
        ng.tosym()
        ng.set_input(a,b,c,d)
        outs2 = ng.forward()
        self.assertEqual(
            outs2, [
                d*d*d*np.cos(a/b+c)/b,
                -d*d*d*np.cos(a/b+c)*a/b/b,
                d*d*d*np.cos(a/b+c),
                3*d*d*np.sin(a/b+c),
            ])

import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestCnd(unittest.TestCase):
    def test_cnd(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.multiply(v1, od.negative(v2))
        v4 = od.sin(v0)
        v5 = od.power(od.add(v1, od.scalar(-1)), od.scalar(2))
        v6 = od.multiply(od.scalar(2), od.subtract(v1, od.scalar(1)))
        v7 = od.lessthan(v3, v4, v5, v6)
        v8 = od.multiply(v7, v2)
        g = Graph([v0, v1, v2], [v8])
        g.pre_optimize()
        # g.to_sym()
        for _ in range(10000):
            inp_size = g.get_inp_size()
            datas = random_array([inp_size], low=-1000.0, high=1000.0)
            a, b, c = datas
            rets = [
                c*((b-1)**2 if b*(-c) < np.sin(a) else 2*(b-1))
            ]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=14)
        dg = g.autograph_backward()
        # dg.toscalar()
        dg.degenerate()
        dg.fuse()
        # dg.to_sym()
        for _ in range(10000):
            inp_size = g.get_inp_size()
            datas = random_array([inp_size], low=-1000.0, high=1000.0)
            a, b, c = datas
            rets = [0, 2*c*(b-1), (b-1)**2] if -b*c < np.sin(a) else \
                [0, 2*c, 2*(b-1)]
            dg.set_input(*datas)
            outs = dg.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=14)

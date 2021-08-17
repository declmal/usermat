import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array


@register_test
class TestCnd(unittest.TestCase):
    def test_cnd(self):
        v0 = od.var("x")
        v1 = od.var("y")
        v2 = od.var("z")
        v3 = od.multiply(v1, od.negative(v2))
        v4 = od.sin(v0)
        v5 = od.power(od.add(v1, od.scalar(-1)), od.scalar(2))
        v6 = od.multiply(od.scalar(2), od.subtract(v1, od.scalar(1)))
        v7 = od.lessthan(v3, v4, v5, v6)
        v8 = od.multiply(v7, v2)
        g = Graph([v0, v1, v2], [v8])
        g.unify()
        g.tosym()
        for _ in range(10000):
            datas = random_array([3], low=-1000.0, high=1000.0)
            a, b, c = datas
            rets = [
                c*((b-1)**2 if b*(-c) < np.sin(a) else 2*(b-1))
            ]
            outs = g.forward(*datas)
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=14)
        dg = g.autodiff()
        dg.optimize()
        dg.tosym()
        for _ in range(10000):
            datas = random_array([3], low=-1000.0, high=1000.0)
            a, b, c = datas
            rets = [0, 2*c*(b-1), (b-1)**2] if -b*c < np.sin(a) else \
                [0, 2*c, 2*(b-1)]
            outs = dg.forward(*datas)
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=14)

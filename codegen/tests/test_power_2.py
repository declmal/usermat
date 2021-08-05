import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestPower2(unittest.TestCase):
    def test_power_2(self):
        v0 = od.var()
        v1 = od.multiply(v0, v0)
        v2 = od.power(v1, od.scalar(Fraction(1,4)))
        v3 = od.multiply(v1, v2)
        v4 = od.power(v3, od.scalar(Fraction(1,4)))
        g = Graph([v0], [v4])
        g.fuse()
        g.to_sym()
        for _ in range(10000):
            datas = random_array([1], low=-100.0, high=100.0)
            a = datas[0]
            rets = [np.abs(a)**(5/8)]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)
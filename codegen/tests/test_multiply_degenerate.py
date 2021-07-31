import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestMultiplyDegenerate(unittest.TestCase):
    def test_multiply_degenerate(self):
        v0 = od.scalar(1)
        v1 = od.var()
        v2 = od.multiply(v0, v1)
        g = Graph([v1], [v2])
        g.degenerate()
        for _ in range(10000):
            datas = random_array([1], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

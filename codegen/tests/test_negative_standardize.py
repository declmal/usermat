import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestNegativeStandardize(unittest.TestCase):
    def test_negative_standardize(self):
        v0 = od.var()
        v1 = od.negative(v0)
        g = Graph([v0], [v1])
        g.standardize()
        for _ in range(10000):
            inp_size = g.get_inp_size()
            datas = random_array([inp_size], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [-a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)
import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.op_def import OpDef as od
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.test_utils import register_test


@register_test
class TestNegativeStandardize(unittest.TestCase):
    def test_negative_standardize(self):
        v0 = od.var()
        v1 = od.negative(v0)
        g = Graph([v0], [v1])
        g.standardize()
        for _ in range(10000):
            datas = random_array([1], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [-a]
            outs = g.forward(*datas)
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

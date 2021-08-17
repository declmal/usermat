import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..utils.type_utils import Zero
from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array


@register_test
class TestDivideStandardize(unittest.TestCase):
    def test_divide_standardize(self):
        v0 = od.var("x")
        v1 = od.var("y")
        v2 = od.divide(v0, v1)
        g = Graph([v0, v1], [v2])
        g.standardize()
        for _ in range(10000):
            flag = True
            while flag:
                datas = random_array([2], low=-1000.0, high=1000.0)
                flag = datas[1] == Zero
            a, b = datas
            rets = [a/b]
            outs = g.forward(*datas)
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

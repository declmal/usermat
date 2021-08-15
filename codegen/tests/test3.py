import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array


@register_test
class Test3(unittest.TestCase):
    def test_3(self):
        v0 = od.var()
        minus_one = od.scalar(-1)
        v00 = od.add(v0, minus_one)
        v1 = od.var()
        v2 = od.var()
        v3 = od.add(v00, v1)
        v4 = od.multiply(v0, v1)
        v5 = od.sin(v4)
        v6 = od.add(v1, v00)
        v7 = od.multiply(v5, v6)
        v8 = od.divide(v7, v2)
        g = Graph([v0, v1, v2], [v8])
        data = random_array([3], low=1000.0, high=123111443.0)
        self.info(
            "set input data: \n{}\n <end of input data>".format(data))
        outs = g.forward(*data)
        a, b, c = data
        self.assertAlmostEqual(outs[0], np.sin(a*b)*((a-np.float64(1))+b)/c)
        g.tosym()
        g.standardize()
        g.tosym()
        a, b, c = data
        outs = g.forward(*data)
        self.assertAlmostEqual(
            outs[0], np.sin(a*b)*((a-np.float64(1))+b)/c, places=10)

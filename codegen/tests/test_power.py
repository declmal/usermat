import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array


@register_test
class TestPower(unittest.TestCase):
    def test_power(self):
        v0 = od.var("x")
        v1 = od.var("y")
        v2 = od.power(v0, od.scalar(Fraction(2,3)))
        v3 = od.power(v1, od.scalar(Fraction(4,5)))
        v4 = od.multiply(v2, v3)
        v5 = od.power(v4, od.scalar(2))
        v6 = od.power(v5, od.scalar(Fraction(1,2)))
        v7 = od.var("z")
        v8 = od.power(v7, od.scalar(2))
        v9 = od.multiply(v6, v8)
        v10 = od.power(v9, od.scalar(Fraction(1,2)))
        g = Graph([v0,v1,v7], [v10])
        g.fuse()
        g.tosym()

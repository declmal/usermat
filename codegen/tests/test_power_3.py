import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero, ExpContradictError
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestPower3(unittest.TestCase):
    def test_power_3(self):
        v0 = od.var()
        v1 = od.multiply(v0, v0)
        v2 = od.abs(v0)
        v3 = od.multiply(v1, v2)
        v4 = od.var()
        v5 = od.power(v4, od.scalar(Fraction(1,2)))
        v6 = od.multiply(v5, v3)
        v7 = od.negative(v6)
        v8 = od.power(v7, od.scalar(Fraction(1,2)))
        g = Graph([v0,v4], [v8])
        g.forward_optimize()
        self.assertRaises(ExpContradictError, g.fuse)

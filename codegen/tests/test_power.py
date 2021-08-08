import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.op_def import OpDef as od
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class TestPower(unittest.TestCase):
    def test_power(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.power(v0, od.scalar(Fraction(2,3)))
        v3 = od.power(v1, od.scalar(Fraction(4,5)))
        v4 = od.multiply(v2, v3)
        v5 = od.power(v4, od.scalar(2))
        v6 = od.power(v5, od.scalar(Fraction(1,2)))
        v7 = od.var()
        v8 = od.power(v7, od.scalar(2))
        v9 = od.multiply(v6, v8)
        v10 = od.power(v9, od.scalar(Fraction(1,2)))
        g = Graph([v0,v1,v7], [v10])
        g.fuse()
        # g.to_sym()

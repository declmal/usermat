import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..op_def import OpDef as od
from ..graph import Graph
from ..type_utils import ContradictError, MinusOne
from .test_utils import register_test, random_array


@register_test
class TestPower3(unittest.TestCase):
    def test_power_3(self):
        v0 = od.var("x")
        v1 = od.multiply(v0, v0)
        v2 = od.abs(v0)
        v3 = od.multiply(v1, v2)
        v4 = od.var("y")
        v5 = od.power(v4, od.scalar(Fraction(1,2)))
        v6 = od.multiply(v5, v3)
        v7 = od.negative(v6)
        v8 = od.power(v7, od.scalar(Fraction(1,2)))
        v9 = od.multiply(v0, od.scalar(MinusOne))
        v10 = od.power(v9, od.scalar(Fraction(1,3)))
        v11 = od.add(v8, v10)
        g = Graph([v0,v4], [v11])
        g.optimize()
        g.merge()
        g.assert_graph()
        g.tosym()
        # reference graph
        v0 = od.var("x")
        v1 = od.var("y")
        half = od.scalar(Fraction(1,2))
        three = od.scalar(Fraction(3,1))
        one = od.scalar(Fraction(1,1))
        v2 = od.monomial(one, v1, half, v0, three)
        v3 = od.assertzero(v2)
        minusone = od.scalar(Fraction(-1,1))
        v4 = od.monomial(minusone, v0, one)
        third = od.scalar(Fraction(1,3))
        v5 = od.monomial(one, v4, third)
        v6 = od.assertnonnegative(v1)
        v7 = od.assertnonpositive(v0)
        g2 = Graph([v0,v1], [v5], asserts=[v6,v7,v3])
        # assertion
        self.assertEqual(g, g2)

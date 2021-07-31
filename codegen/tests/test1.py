import unittest
from os import path
from fractions import Fraction

import numpy as np

from codegen.ops import OpDef as od, Zero
from codegen.graph import Graph
from codegen.utils import random_array
from codegen.tests.test_utils import register_test


@register_test
class Test1(unittest.TestCase):
    def test_1(self):
        a = od.var()
        b = od.var()
        c = od.add(a, b)
        d = od.add(b, c)
        g = Graph([a,b], [c,d])
        g.set_input(1, 2)
        outs1 = g.forward()
        self.assertEqual(outs1, [3,5])
        g.reset()
        g.set_input(3, -1)
        outs2 = g.forward()
        self.assertEqual(outs2, [2,1])
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)

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
    def test_power_4(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.power(v0, od.scalar(Fraction(1,3)))
        v3 = od.power(v1, od.scalar(Fraction(3,5)))
        v4 = od.multiply(v2, v3)
        v5 = od.power(v4, od.scalar(Fraction(1,2)))
        v6 = od.multiply(v2, v5)
        v7 = od.multiply(v6, v3)
        v8 = od.power(v7, od.scalar(Fraction(1,2)))
        g = Graph([v0,v1], [v8])
        g.pre_optimize()
        g.fuse()
        g.discrete_exp()
        g.to_sym()
        for _ in range(10000):
            flag = True
            while flag:
                datas = random_array([2], low=-100.0, high=100.0)
                a, b = datas
                flag = a*b < 0
            a, b = datas
            rets = [
                ((-1)**3*(-1)**1*np.abs(b)**(3/5)*np.abs(a)**(1/3))**(3/4)]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

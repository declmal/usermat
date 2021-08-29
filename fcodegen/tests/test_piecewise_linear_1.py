import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..utils.math_utils import get_piecewise_linear_info, get_piecewise_linear_diff_info
from ..utils.type_utils import One
from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array


@register_test
class TestPiecewiseLinear1(unittest.TestCase):
    def test_piecewise_linear_1(self):
        v0 = od.var("x")
        v1 = od.multiply(v0, v0)
        v2 = od.add(v1, od.scalar(One))
        # piecewise linear operator
        end_tuples = [(5,(-2,5)), (0.5,(3,-1))]
        point_tuples = [((-2,4),(1,1)), ((1,-1),(3,-1))]
        points = [(-2,4), (1,0), (3,-2)]
        scalar_datas = get_piecewise_linear_info(
            end_tuples, point_tuples, points)
        scalars = [od.scalar(data) for data in scalar_datas]
        v3 = od.piecewiselinear(v2, *scalars)
        v4 = od.var("y")
        v5 = od.multiply(v4, v3)
        g = Graph([v0, v4], [v5])
        g.unify()
        g.autodiff()
        g.optimize()
        # reference graph validate
        v0 = od.var("x")
        v1 = od.var("y")
        one = od.scalar(One)
        two = od.scalar(2)
        v2 = od.monomial(one, v0, two)
        v3 = od.polynomial(one, v2, one)
        v4 = od.piecewiselinear(v3, *scalars)
        nscalar_datas = get_piecewise_linear_diff_info(*scalar_datas)
        nscalars = [od.scalar(data) for data in nscalar_datas]
        v5 = od.piecewiselinear(v3, *nscalars)
        v6 = od.monomial(two, v0, one, v1, one, v5, one)
        gref = Graph([v0,v1], [v6,v4])
        self.assertEqual(gref, g)
        # reference pl funciton
        def fref(x):
            if x < -2:
                ret = 5*x + 15
            elif x < 1:
                ret = -x + 2
            elif x == 1:
                ret = 0
            elif x < 3:
                ret = -1
            elif x == 3:
                ret = -2
            else:
                ret = 0.5*x - 2.5
            return ret
        # reference pl diff
        def dfref(x):
            if x < -2:
                ret = 5
            elif x < 1:
                ret = -1
            elif x == 1:
                ret = -0.5
            elif x < 3:
                ret = 0
            elif x == 3:
                ret = 0.25
            else:
                ret = 0.5
            return ret
        # value validate
        for _ in range(1000):
            xf, y = random_array([2], low=-4.0, high=6.0)
            xs = [xf, -2, 1, 3]
            ind = np.random.randint(len(xs))
            x = xs[ind]
            outs = g.forward(x, y)
            ref_outs = [2*dfref(x**2+1)*x*y, fref(x**2+1)]
            self.assertEqual(outs, ref_outs)

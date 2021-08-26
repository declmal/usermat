import unittest
from os import path
from fractions import Fraction

import numpy as np

from ..utils.math_utils import get_piecewise_linear_info
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
        ends = [
            (-4.5,-2), (-3.5,1), (-3,2), (-2,0.5), (0,0.5), (1,0), (3,1)]
        k = -2
        k1 = 0
        end_tuples = [(k, ends[0]), (k1, ends[-1])]
        point_tuples = [
            (ends[0], ends[1]),
            (ends[1], ends[2]),
            (ends[2], ends[3]),
            (ends[3], ends[4]),
            (ends[4], ends[5]),
            (ends[5], ends[6]),
        ]
        points = ends.copy()
        points[4] = (0,-5)
        scalar_datas = get_piecewise_linear_info(
            end_tuples, point_tuples, points)
        scalars = [od.scalar(data) for data in scalar_datas]
        v3 = od.piecewiselinear(v2, *scalars)
        g = Graph([v0], [v3])
        g.unify()
        g.autodiff()
        g.optimize()
        g.tosym()

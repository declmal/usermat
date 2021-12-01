import unittest

from ..utils.math_utils import get_piecewise_linear_info, get_piecewise_linear_diff_info
from .test_utils import register_test


@register_test
class Test(unittest.TestCase):
    def test_get_piecewise_linear_info(self):
        end_tuples = [(5,(-2,5)), (0.5,(3,-1))]
        point_tuples = [((-2,4),(1,1)), ((1,-1),(3,-1))]
        points = [(-2,4), (1,0), (3,-2)]
        ref_datas = [5,15,-2,4,-1,2,1,0,0,-1,3,-2,0.5,-2.5]
        datas = get_piecewise_linear_info(end_tuples, point_tuples, points)
        self.assertEqual(ref_datas, datas)
        ndatas = get_piecewise_linear_diff_info(*datas)
        nref_datas = [0,5,-2,-1,0,-1,1,-0.5,0,0,3,0.25,0,0.5]
        self.assertEqual(nref_datas, ndatas)

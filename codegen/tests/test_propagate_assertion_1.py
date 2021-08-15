import unittest
from fractions import Fraction

from ..type_utils import One
from ..op_def import OpDef as od
from ..graph import Graph
from .test_utils import register_test, random_array

@register_test
class TestPropagateAssertion1(unittest.TestCase):
    def test_propagate_assertion_1(self):
        u = od.var("x")
        v = od.var("y")
        x = od.multiply(u, v)
        y = od.power(x, od.scalar(Fraction(1,2)))
        z = od.divide(y, u)
        w = od.subtract(z, od.scalar(One))
        g = Graph([u,v], [w])
        g.standardize()
        g.degenerate()
        g.tosym()
        # g.propagate_assertion()
        for _ in range(10000):
            datas = random_array([2], low=-1000.0, high=1000.0)
            a, b = datas
            rets = [(a*b)**0.5/a-1]
            outs = g.forward(*datas)
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

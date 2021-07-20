import unittest

from codegen.graph_def import GraphDef as gd

class TestOps(unittest.TestCase):
    def test_add(self):
        a = gd.var()
        b = gd.var()
        c = gd.add(a, b)
        d = gd.add(b, a)
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)

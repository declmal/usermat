import unittest

from codegen.ops import Var, Add

class TestScalar(unittest.TestCase):
    def test_add(self):
        a = Var()
        b = Var()
        c = Add(a, b)
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)

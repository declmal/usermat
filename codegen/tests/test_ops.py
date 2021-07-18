import unittest

from codegen.symbol import Sym, Add

class TestScalar(unittest.TestCase):
    def test_add(self):
        a = Sym()
        b = Sym()
        c = Add(a, b)
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)

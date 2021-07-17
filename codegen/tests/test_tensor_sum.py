import unittest

from codegen.tensor import Tensor

class TestTensorSum(unittest.TestCase):
    def test_simple_sum(self):
        t1 = Tensor([1,2,3], requires_grad=True)
        t2 = t1.sum()
        t2.backward()
        self.assertEqual(
            t1.grad.data.tolist(),
            [1,1,1]
        )

    def test_simple_sum_grad(self):
        t1 = Tensor([2,2,3], requires_grad=True)
        t2 = t1.sum()
        t2.backward(Tensor(3))
        self.assertEqual(
            t1.grad.data.tolist(),
            [3,3,3]
        )

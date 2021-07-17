from typing import List, NamedTuple, Callable, Optional, Union

import numpy as np


class Dependency(NamedTuple):
    tensor: 'Tensor'
    grad_fn: Callable[['np.ndarray'], 'np.ndarray']

Arrayable = Union['float', 'int', 'list', 'np.number', 'np.ndarray']

def ensure_array(arrayable: Arrayable) -> np.ndarray:
    if isinstance(arrayable, np.ndarray):
        return arrayable
    else:
        return np.array(arrayable)


class Tensor:
    grad: Optional['Tensor']

    def __init__(
        self,
        data: Arrayable,
        requires_grad: bool=False,
        depends_on: list=[]
    ) -> None:
        self.data = ensure_array(data)
        self.requires_grad = requires_grad
        self.depends_on = depends_on
        self.shape = self.data.shape
        if self.requires_grad:
            self.zero_grad()

    def __repr__(self) -> str:
        return f"Tensor({self.data}, requires_grad={self.requires_grad})"

    def zero_grad(self) -> None:
        self.grad = Tensor(np.zeros_like(self.data))

    def backward(self, grad: Optional['Tensor']=None) -> None:
        assert self.requires_grad, "called backward on non-requires-grad tensor"
        if grad is None:
            if self.shape == ():
                grad = Tensor(1)
            else:
                raise RuntimeError("grad must be specified for non-0-tensor")
        self.grad.data += grad.data
        for dependency in self.depends_on:
            backward_grad = dependency.grad_fn(grad.data)
            dependency.tensor.backward(Tensor(backward_grad))

    def sum(self) -> 'Tensor':
        return tensor_sum(self)

def tensor_sum(t: 'Tensor') -> 'Tensor':
    """
    Takes a tensor and returns the 0-tensor
    that's the sum of all its elements.
    """
    data = ensure_array(t.data.sum())
    requires_grad = t.requires_grad
    if requires_grad:
        def grad_fn(grad: 'np.ndarray') -> 'np.ndarray':
            """
            grad is neceseary for a 0-tensor, so each input element
            contributes that much
            """
            return grad * np.ones_like(t.data)
        depends_on = [Dependency(t, grad_fn)]
    else:
        depends_on = []

    return Tensor(
        data,
        requires_grad,
        depends_on)



class Operator(object):
    pass

class Node(object):
    op: 'Operator'
    def __init__(self,op: Operator) -> None:
        self.op = op



def forward():
    pass

def autodiff_graph_reverse():
    pass

def autodiff_graoh_forward():
    pass

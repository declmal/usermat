import unittest
import logging
import inspect

import numpy as np

from codegen.ops import OpDef as od
from codegen.graph import Graph
from codegen.utils import random_array

def register_logger(cls):
    def info_func(self, info_s: str) -> None:
        return self.logger.info(info_s)

    assert "info" not in dir(cls)
    setattr(cls, "info", info_func)

    def register_logger_func(func):
        def wrapper(self, *args, **kwargs):
            self.logger = logging.getLogger(inspect.stack()[0][3])
            return func(self, *args, **kwargs)
        return wrapper

    for func_name in dir(cls):
        if not func_name.startswith("test_"):
            continue
        func = getattr(cls, func_name)
        func = register_logger_func(func)
        setattr(cls, func_name, func)
    return cls


@register_logger
class TestOps(unittest.TestCase):
    def test_1(self):
        od.reset()
        a = od.var()
        b = od.var()
        c = od.add(a, b)
        d = od.add(b, c)
        g = Graph([a,b], [c,d])
        g.set_input(1, 2)
        outs1 = g.forward()
        self.assertEqual(outs1, [3,5])
        g.reset()
        g.set_input(3, -1)
        outs2 = g.forward()
        self.assertEqual(outs2, [2,1])
        c.backward(grad=1)
        self.assertEqual(b.grad, 1)

    def test_2(self):
        od.reset()
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.var()
        v4 = od.divide(v0, v1)
        v5 = od.add(v4, v2)
        v6 = od.sin(v5)
        v7 = od.multiply(v3, v3)
        v8 = od.multiply(v7, v3)
        v9 = od.multiply(v6, v8)
        g = Graph([v0,v1,v2,v3], [v9])
        a, b, c, d = [float(v) for v in [1, 2, 3, 4]]
        g.set_input(a,b,c,d)
        outs1 = g.forward()
        self.assertEqual(outs1, [np.sin(a/b+c)*d*d*d])
        # g.to_sym()
        ng = g.autograph_backward()
        ng.to_sym()
        ng.set_input(a,b,c,d)
        outs2 = ng.forward()
        self.assertEqual(
            outs2, [
                d*d*d*np.cos(a/b+c)/b,
                -d*d*d*np.cos(a/b+c)*a/b/b,
                d*d*d*np.cos(a/b+c),
                3*d*d*np.sin(a/b+c),
            ])

    def test_3(self):
        od.reset()
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.add(v0, v1)
        v4 = od.multiply(v0, v1)
        v5 = od.sin(v4)
        v6 = od.add(v1, v0)
        v7 = od.multiply(v5, v6)
        g = Graph([v0, v1, v2], [v7])
        data = random_array([3])
        self.info("set input data: \n{}\n <end of input data>".format(data))
        g.set_input(*data)
        outs = g.forward()
        a, b, c = data
        self.assertEqual(outs, np.sin(a*b)*(a+b))
        # g.to_sym()

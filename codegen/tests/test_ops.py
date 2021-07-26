import unittest
import logging
from os import path

import numpy as np

from codegen.ops import OpDef as od
from codegen.graph import Graph
from codegen.utils import random_array

def register_test(cls):
    def info_func(self, s: str) -> None:
        return self.logger.info(s)
    def warn_func(self, s: str) -> None:
        return self.logger.warning(s)

    assert "info" not in dir(cls)
    setattr(cls, "info", info_func)
    assert "warn" not in dir(cls)
    setattr(cls, "warn", warn_func)

    def register_test_func(func):
        def wrapper(self, *args, **kwargs):
            self.logger = logging.getLogger(func.__name__)
            self.warn("starting")
            od.reset(clear_scalar=True)
            ret = func(self, *args, **kwargs)
            self.warn("end")
            return ret
        return wrapper

    for func_name in dir(cls):
        if not func_name.startswith("test_"):
            continue
        func = getattr(cls, func_name)
        func = register_test_func(func)
        setattr(cls, func_name, func)
    return cls


@register_test
class TestOps(unittest.TestCase):
    def test_1(self):
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
        v0 = od.var()
        minus_one = od.scalar(-1)
        v00 = od.add(v0, minus_one)
        v1 = od.var()
        v2 = od.var()
        v3 = od.add(v00, v1)
        v4 = od.multiply(v0, v1)
        v5 = od.sin(v4)
        v6 = od.add(v1, v00)
        v7 = od.multiply(v5, v6)
        v8 = od.divide(v7, v2)
        g = Graph([v0, v1, v2], [v8])
        data = random_array([3], low=1000.0, high=123111443.0)
        self.info(
            "set input data: \n{}\n <end of input data>".format(data))
        g.set_input(*data)
        outs = g.forward()
        a, b, c = data
        self.assertAlmostEqual(outs[0], np.sin(a*b)*((a-np.float64(1))+b)/c)
        g.to_sym()
        g.rewrite()
        g.to_sym(json_path=path.expanduser("~/Desktop/mx2.json"))
        a, b, c = data
        g.set_input(*data)
        outs = g.forward()
        self.assertAlmostEqual(outs[0], np.sin(a*b)*((a-np.float64(1))+b)/c, places=10)

    def test_mul(self):
        pass

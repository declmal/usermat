import unittest
import logging
from os import path

import numpy as np

from codegen.ops import OpDef as od, Zero
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
            od.reset()
            ret = func(self, *args, **kwargs)
            self.warn("succeed")
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
        g.to_sym()
        g.pre_optimize()
        g.to_sym()
        a, b, c, d = [float(v) for v in [1, 2, 3, 4]]
        g.set_input(a,b,c,d)
        outs1 = g.forward()
        self.assertEqual(outs1, [np.sin(a/b+c)*d*d*d])
        ng = g.autograph_backward()
        ng.fuse()
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
        # g.to_sym()
        g.standardize()
        # g.to_sym(json_path=path.expanduser("~/Desktop/mx2.json"))
        a, b, c = data
        g.set_input(*data)
        outs = g.forward()
        self.assertAlmostEqual(outs[0], np.sin(a*b)*((a-np.float64(1))+b)/c, places=10)

    def test_mul(self):
        pass

    def test_add_degenerate(self):
        v0 = od.scalar(0)
        v1 = od.var()
        v2 = od.add(v0, v1)
        g = Graph([v1], [v2])
        g.degenerate()
        for _ in range(10000):
            datas = random_array([1], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

    def test_multiply_degenerate(self):
        v0 = od.scalar(1)
        v1 = od.var()
        v2 = od.multiply(v0, v1)
        g = Graph([v1], [v2])
        g.degenerate()
        for _ in range(10000):
            datas = random_array([1], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

    def test_power_degenerate(self):
        v0 = od.scalar(1)
        v1 = od.var()
        v2 = od.power(v1, v0)
        g = Graph([v1], [v2])
        g.degenerate()
        for _ in range(10000):
            datas = random_array([1], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

    def test_divide_standardize(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.divide(v0, v1)
        g = Graph([v0, v1], [v2])
        g.standardize()
        for _ in range(10000):
            flag = True
            while flag:
                datas = random_array([2], low=-1000.0, high=1000.0)
                flag = datas[1] == Zero
            a, b = datas
            rets = [a/b]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

    def test_negative_standardize(self):
        v0 = od.var()
        v1 = od.negative(v0)
        g = Graph([v0], [v1])
        g.standardize()
        for _ in range(10000):
            inp_size = g.get_inp_size()
            datas = random_array([inp_size], low=-1000.0, high=1000.0)
            a = datas[0]
            rets = [-a]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)

    def test_cnd_op(self):
        v0 = od.var()
        v1 = od.var()
        v2 = od.var()
        v3 = od.multiply(v1, od.negative(v2))
        v4 = od.sin(v0)
        v5 = od.power(od.add(v1, od.scalar(-1)), od.scalar(2))
        v6 = od.multiply(od.scalar(2), od.subtract(v1, od.scalar(1)))
        v7 = od.lessthan(v3, v4, v5, v6)
        v8 = od.multiply(v7, v2)
        g = Graph([v0, v1, v2], [v8])
        g.pre_optimize()
        g.to_sym()
        for _ in range(10000):
            inp_size = g.get_inp_size()
            datas = random_array([inp_size], low=-1000.0, high=1000.0)
            a, b, c = datas
            rets = [
                c*((b-1)**2 if b*(-c) < np.sin(a) else 2*(b-1))
            ]
            g.set_input(*datas)
            outs = g.forward()
            for i in range(len(rets)):
                self.assertAlmostEqual(outs[i], rets[i], places=10)
        dg = g.autograph_backward()


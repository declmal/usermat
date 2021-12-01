import unittest
from os import path

import matplotlib.pyplot as plt
import numpy as np

from .test_utils import register_test
from ..op_def import OpDef as od
from ..utils.type_utils import One, Zero, MinusOne
from ..utils.math_utils import get_piecewise_linear_info_consec
from ..graph import Graph

default_nsegs = 4

def estimate_peak_bond_stress(fcp):
    tau_max_data = 1.163 * fcp ** 0.75
    return tau_max_data

def estimate_peak_bond_stress_slip(db):
    s_peak_data = 0.07 * db
    return s_peak_data

def estimate_clear_spacing(db, ratio=0.5):
    sR_data = ratio * db
    return sR_data

def polygen(func, x0, x1, nsegs=default_nsegs):
    assert x0 < x1, "x0: {}, x1: {}".format(x0, x1)
    inc = (x1-x0) / nsegs
    xs = [x0+inc*i for i in range(1, nsegs)]
    pointlist = [(x, func(x)) for x in xs]
    return pointlist

def estimate_yielding_strain(fy, E):
    eps_y = fy / E
    return eps_y

def estimate_rib_height(db):
    hR = 0.2 * db
    return hR

def bond_stress_ref(s2, tau_max, s_peak, sR):
    assert sR > 1.6 * s_peak
    if s2 < -sR:
        tau_b = 0
    elif s2 < -1.6 * s_peak:
        tau_b = -0.6 * tau_max * (1-(abs(s2)-1.6*s_peak)/(sR-1.6*s_peak))
    elif s2 < -1.5 * s_peak:
        tau_b = -0.6 * tau_max
    elif s2 < -0.15 * s_peak:
        tau_b = -tau_max * (0.6-0.36*((abs(s2)-1.5*s_peak)/(1.35*s_peak))**4)
    elif s2 < 0:
        tau_b = -1.6 * tau_max * abs(s2) / s_peak
    elif s2 < 0.1 * s_peak:
        tau_b = 3 * tau_max * s2 / s_peak
    elif s2 < s_peak:
        tau_b = tau_max * (0.75-0.45*((s2-s_peak)/(0.9*s_peak))**4)
    elif s2 < 1.1 * s_peak:
        tau_b = 0.75 * tau_max
    elif s2 < sR:
        tau_b = 0.75 * tau_max * (1-(s2-1.1*s_peak)/(sR-1.1*s_peak))
    else:
        tau_b = 0
    return tau_b

def friction_stress_ref(s2, tau_max, s_peak):
    if s2 < -1.5 * s_peak:
        tau_f = -0.25 * tau_max
    elif s2 < -0.15 * s_peak:
        tau_f = -tau_max * (0.25-0.15*((abs(s2)-1.5*s_peak)/(1.35*s_peak))**4)
    elif s2 < 0:
        tau_f = -2/3 * tau_max * abs(s2) / s_peak
    elif s2 < 0.1 * s_peak:
        tau_f = tau_max * s2 / s_peak
    elif s2 < s_peak:
        tau_f = tau_max * (0.25-0.15*((s2-s_peak)/(0.9*s_peak))**4)
    else:
        tau_f = 0.25 * tau_max
    return tau_f

def bearing_stress(
    s2_op, tau_max, s_peak, sR, nsegs1=default_nsegs, nsegs2=default_nsegs):
    """
    """
    assert sR > 1.6 * s_peak
    # scalars
    points = [
        (-sR/s_peak, 0),
        (-1.6, -0.6),
        (-1.5, -0.6),
        *polygen(
            lambda x: -(0.6 - 0.36 * ((-x-1.5)/1.35)**4),
            -1.5, -0.15, nsegs=nsegs1),
        (-0.15, -1.6*0.15),
        (0, 0),
        (0.1, 3*0.1),
        *polygen(
            lambda x: 0.75 - 0.45 * ((x-1)/0.9)**4,
            0.1, 1, nsegs=nsegs2),
        (1, 0.75),
        (1.1, 0.75),
        (sR/s_peak, 0),
    ]
    for i in range(len(points)):
        points[i] = (points[i][0], points[i][1]*tau_max)
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    # ret
    ratio = od.divide(s2_op, od.scalar(s_peak))
    tau_b = od.piecewiselinear(ratio, *scalar_ops)
    return tau_b

def friction_stress(
    s2_op, tau_max, s_peak, nsegs1=default_nsegs, nsegs2=default_nsegs):
    """ The
    """
    # scalars
    points = [
        (-1.5, -0.25),
        *polygen(
            lambda x: -(0.25 - 0.15 * ((-x-1.5)/1.35)**4),
            -1.5, -0.15, nsegs=nsegs1),
        (-0.15, -0.1),
        (0, 0),
        (0.1, 0.1),
        *polygen(
            lambda x: 0.25 - 0.15 * ((x-1)/0.9)**4,
            0.1, 1, nsegs=nsegs2),
        (1, 0.25),
    ]
    for i in range(len(points)):
        points[i] = (points[i][0], points[i][1]*tau_max)
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    # ret
    ratio = od.divide(s2_op, od.scalar(s_peak))
    tau_f = od.piecewiselinear(ratio, *scalar_ops)
    return tau_f

def spliting_reduction(s1_op, hR):
    """
    """
    points = [(0.5*hR, 1), (hR, 0)]
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    rho_n_op = od.piecewiselinear(s1_op, *scalar_ops)
    return rho_n_op

def yielding_reduction(eps_s_op, eps_y, eps_sh):
    """
    """
    assert eps_y < eps_sh, "eps_y: {}, eps_sh: {}".format(eps_y, eps_sh)
    points = [(eps_y, 1), (eps_sh, 0)]
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    rho_bs_op = od.piecewiselinear(eps_s_op, *scalar_ops)
    return rho_bs_op

def hardening_reduction(eps_s_op, eps_sh, eps_u):
    """
    """
    assert eps_sh < eps_u, "eps_sh: {}, eps_u: {}".format(eps_sh, eps_u)
    points = [(eps_sh, 1), (eps_u, 0)]
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    rho_fs_op = od.piecewiselinear(eps_s_op, *scalar_ops)
    return rho_fs_op

def cyclic_bear_reduction_ref(savg_max, sR):
    rho_bc = min(1.2*np.exp(-2.7*(savg_max/sR)**0.8), 1)
    return rho_bc

def cyclic_friction_reduction_ref(sp_max, sn_max, scum, sR):
    rho_fc = 1 - min((sp_max+sn_max)/sR, 1) * \
        (1-np.exp(-0.45*(scum/sR)**0.75))
    return rho_fc

def cyclic_bear_reduction(savg_max_op, sR, nsegs=default_nsegs):
    """
    """
    cutoff = -np.log(1/1.2)
    points = [
        (0, 1),
        (cutoff, 1),
        *polygen(lambda x: 1.2*np.exp(-x), cutoff, 7, nsegs=nsegs),
        (7, 0),
    ]
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    _power = od.power(savg_max_op, od.scalar(0.8))
    _mul = od.multiply(od.scalar(2.7/sR**0.8), _power)
    rho_bc_op = od.piecewiselinear(_mul, *scalar_ops)
    return rho_bc_op

def cyclic_friction_reduction(
    sp_max_op, sn_max_op, scum_op, sR, nsegs=default_nsegs):
    """
    """
    points = [
        (0, 0),
        *polygen(lambda x: 1-np.exp(-x), 0, 7, nsegs=nsegs),
        (7, 1),
    ]
    scalars = get_piecewise_linear_info_consec(points, 0, 0)
    scalar_ops = [od.scalar(scalar) for scalar in scalars]
    _power = od.power(scum_op, od.scalar(0.75))
    _mul = od.multiply(od.scalar(0.45/sR**0.75), _power)
    _piecewise = od.piecewiselinear(_mul, *scalar_ops)
    _add = od.add(sp_max_op, sn_max_op)
    _div = od.divide(_add, od.scalar(sR))
    one = od.scalar(1)
    _min = od.min(_div, one)
    _mul2 = od.multiply(_min, _piecewise)
    rho_fc_op = od.subtract(one, _mul2)
    return rho_fc_op

def bond_stress(
    tau_b, tau_f, rho_n, rho_bs, rho_fs, rho_bc, rho_fc):
    """
    """
    mono1 = od.monomial(one, rho_bs, one, rho_bc, one, tau_b, one)
    mono2 = od.monomial(one, rho_fs, one, rho_fc, one, tau_f, one)
    poly = od.polynomial(zero, mono1, one, mono2, one)
    tau2 = od.monimial(one, rho_n, one, poly, one)
    return tau2

def normal_stress(tau2, theta, K_pen1, s1):
    op1 = od.abs(tau2)
    op2 = od.tan(theta)
    mono1 = od.monomial(minus_one, op1, one, op2, one)
    op3 = od.min(s1, zero)
    mono2 = od.monomial(one, K_pen1, one, op3, one)
    sig1 = od.polynomial(zero, mono1, one, mono2, one)
    return sig1

def transverse_tangential_stress(K_pen3, s3):
    """ The rotation of the bar arhond its longitudinal axis is restrained.

        Parameters
        ----------
        K_pen3: fcodegen.op.zero_deps.Scalar
            A penalty stiffness.
        s3: fcodegen.op.base.Op
            Transverse relative displacement.

        Returns
        -------
        tau3: fcodegen.op.base.Op
            Transverse tangential stress.
    """
    tau3 = od.monomial(one, K_pen3, one, s3, one)
    return tau3

def plot_fig(*args, fname="fig.png", fdir=path.expanduser("~/Desktop")):
    fpath = path.join(fdir, fname)
    for i in range(0, len(args), 2):
        x, y = args[i:i+2]
        plt.plot(x, y)
    fig = plt.gcf()
    fig.savefig(fpath)
    plt.close()

def gen_data(func, low, high, num_points=1000):
    assert low < high, "low: {}, high: {}".format(low, high)
    xs = np.linspace(low, high, num_points)
    ys = []
    for x in xs:
        y = func(x)
        ys.append(y)
    return xs, ys

@register_test
class TestBSIM(unittest.TestCase):
    def test_bond_stress(self):
        fcp = 40
        db = 18
        tau_max = estimate_peak_bond_stress(fcp)
        s_peak = estimate_peak_bond_stress_slip(db)
        sR = estimate_clear_spacing(db)
        s2_op = od.var("var")
        tau_b_op = bearing_stress(s2_op, tau_max, s_peak, sR)
        g = Graph([s2_op], [tau_b_op])

        s2_lst, tau_b_lst = gen_data(g.forward, -2*sR, 2*sR)
        s2_lst_ref, tau_b_lst_ref = gen_data(
            lambda x: bond_stress_ref(x, tau_max, s_peak, sR),
            -2*sR, 2*sR)
        plot_fig(
            s2_lst, tau_b_lst, s2_lst_ref, tau_b_lst_ref,
            fname="bond_stress.png")

    def test_friction_stress(self):
        fcp = 40
        db = 18
        tau_max = estimate_peak_bond_stress(fcp)
        s_peak = estimate_peak_bond_stress_slip(db)
        s2_op = od.var("var")
        tau_f_op = friction_stress(s2_op, tau_max, s_peak)
        g = Graph([s2_op], [tau_f_op])

        s2_lst, tau_f_lst = gen_data(g.forward, -2*s_peak, 2*s_peak)
        s2_lst_ref, tau_f_lst_ref = gen_data(
            lambda x: friction_stress_ref(x, tau_max, s_peak),
            -2*s_peak, 2*s_peak)
        plot_fig(
            s2_lst, tau_f_lst, s2_lst_ref, tau_f_lst_ref,
            fname="friction_stress.png")

    def test_spliting_reduction(self):
        db = 18
        hR = estimate_rib_height(db)
        s1_op = od.var("s1")
        rho_n_op = spliting_reduction(s1_op, hR)
        g = Graph([s1_op], [rho_n_op])

        s1_lst, rho_n_lst = gen_data(g.forward, 0, 2*hR)
        plot_fig(s1_lst, rho_n_lst, fname="spliting_reduction.png")

    def test_yielding_reduction(self):
        fy = 400
        E = 206000
        eps_y = estimate_yielding_strain(fy, E)
        eps_sh = 0.01
        eps_s_op = od.var("eps_s")
        rho_bs_op = yielding_reduction(eps_s_op, eps_y, eps_sh)
        g = Graph([eps_s_op], [rho_bs_op])

        eps_s_lst, rho_bs_lst = gen_data(g.forward, 0, 2*eps_sh)
        plot_fig(eps_s_lst, rho_bs_lst, fname="yielding_reduction.png")

    def test_hardening_reduction(self):
        eps_sh = 0.01
        eps_u = 0.15
        eps_s_op = od.var("eps_s")
        rho_fs_op = hardening_reduction(eps_s_op, eps_sh, eps_u)
        g = Graph([eps_s_op], [rho_fs_op])

        eps_s_lst, rho_fs_lst = gen_data(g.forward, 0, 2*eps_u)
        plot_fig(eps_s_lst, rho_fs_lst, fname="hardening_reduction.png")

    def test_cyclic_bear_reduction(self):
        db = 18
        sR = estimate_clear_spacing(db)
        savg_max_op = od.var("savg_max")
        rho_bc_op = cyclic_bear_reduction(savg_max_op, sR, nsegs=9)
        g = Graph([savg_max_op], [rho_bc_op])

        savg_max_lst, rho_bc_lst = gen_data(g.forward, 0, 3*sR)
        savg_max_lst_ref, rho_bc_lst_ref = gen_data(
            lambda x: cyclic_bear_reduction_ref(x, sR), 0, 3*sR)
        plot_fig(
            savg_max_lst, rho_bc_lst, savg_max_lst_ref, rho_bc_lst_ref,
            fname="cyclic_bear_reduction.png")

    def test_cyclic_friction_reduction(self):
        db = 18
        sR = estimate_clear_spacing(db)
        sp_max_op = od.var("sp_max")
        sn_max_op = od.var("sn_max")
        scum_op = od.var("scum")
        rho_fc_op = cyclic_friction_reduction(
            sp_max_op, sn_max_op, scum_op, sR, nsegs=9)
        g = Graph([sp_max_op, sn_max_op, scum_op], [rho_fc_op])

        scum_lst, rho_fc_lst = gen_data(
            lambda x: g.forward(sR, 0, x), 0, 32*sR)
        scum_lst_ref, rho_fc_lst_ref = gen_data(
            lambda x: cyclic_friction_reduction_ref(sR, 0, x, sR), 0, 32*sR)
        plot_fig(
            scum_lst, rho_fc_lst, scum_lst_ref, rho_fc_lst_ref,
            fname="cyclic_friction_reduction.png")

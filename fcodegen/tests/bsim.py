import unittest
from os import path

import matplotlib.pyplot as plt
import numpy as np

from .test_utils import register_test, validate_od_func
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
    s2, tau_max, s_peak, sR, nsegs1=default_nsegs, nsegs2=default_nsegs):
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
    scalar_datas = get_piecewise_linear_info_consec(points, 0, 0)
    scalars = [od.scalar(scalar_data) for scalar_data in scalar_datas]
    # ret
    ratio = od.divide(s2, od.scalar(s_peak))
    tau_b = od.piecewiselinear(ratio, *scalars)
    return tau_b

def friction_stress(
    s2, tau_max, s_peak, nsegs1=default_nsegs, nsegs2=default_nsegs):
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
    scalar_datas = get_piecewise_linear_info_consec(points, 0, 0)
    scalars = [od.scalar(scalar_data) for scalar_data in scalar_datas]
    # ret
    ratio = od.divide(s2, od.scalar(s_peak))
    tau_f = od.piecewiselinear(ratio, *scalars)
    return tau_f

@validate_od_func
def spliting_reduction(s1, hR):
    """
    """
    points = [(0.5, 1), (1, 0)]
    scalar_datas = get_piecewise_linear_info_consec(points, 0, 0)
    scalars = [od.scalar(scalar_data) for scalar_data in scalar_datas]
    ratio = od.divide(s1, hR)
    rho_n = od.piecewiselinear(ratio, *scalars)
    return rho_n

@validate_od_func
def bond_stress(
    tau_b, tau_f, rho_n, rho_bs, rho_fs, rho_bc, rho_fc):
    """ The bond resistance considers both bearing and friction resistance,
        intended for initial loading and loading beyond the maximum slip
        values attained in the precious cycles.

        Parameters
        ----------
        tau_b: fcodegen.op.base.Op
            Full bearing stress of an elastic bar subject to a monotonic
            pullout action.
        tau_f: fcodegen.op.base.Op
            Full friction stress of an elastic bar subject to a monotonic
            pullout action.
        rho_n: fcodegen.op.base.Op
            A bond stress reduction factor that accounts for the opening of
            splitting cracks in concrete.
        rho_bs: fcodegen.op.base.Op
            A bearing stress reduction factor that accounts for the yielding
            of the bar in tension.
        rho_fs: fcodegen.op.base.Op
            A friction stress reduction factor that accounts for the yielding
            of the bar in tension.
        rho_bc: fcodegen.op.base.Op
            A bearing stress reduction factor that accounts for the
            loading-history-dependent bond deterioration.
        rho_fc: fcodegen.op.base.Op
            A friction stress reduction factor that accounts for the
            loading-history-dependent bond deterioration.

        Returns
        -------
        tau2: fcodegen.op.base.Op
            Bond stress.
    """
    mono1 = od.monomial(one, rho_bs, one, rho_bc, one, tau_b, one)
    mono2 = od.monomial(one, rho_fs, one, rho_fc, one, tau_f, one)
    poly = od.polynomial(zero, mono1, one, mono2, one)
    tau2 = od.monimial(one, rho_n, one, poly, one)
    return tau2

@validate_od_func
def normal_stress(tau2, theta, K_pen1, s1):
    """ Normal stress is to simulate the wedging action of the bar ribs.

        Parameters
        ----------
        tau2: fcodegen.op.base.Op
            Bond stress.
        theta: fcodegne.op.zero_deps.Scalar
            A constant inclination angle with respect to the longitudinal axis
            of the bar.
        K_pen1: fcodegen.op.zero_deps.Scalar
            A penalty stiffness introduced to minimize the interpenetration
            of the steel and concrete.
        s1: fcodegen.op.base.Op
            Normal relative displacement.

        Returns
        -------
        sig1: fcodegen.op.base.Op
            Normal stress.
    """
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


@register_test
class TestBSIM(unittest.TestCase):
    def test_stress(self):
        fcp = 40
        db = 18
        tau_max = estimate_peak_bond_stress(fcp)
        s_peak = estimate_peak_bond_stress_slip(db)
        sR = estimate_clear_spacing(db)
        s2_op = od.var("var")
        tau_b_op = bearing_stress(s2_op, tau_max, s_peak, sR)
        tau_f_op = friction_stress(s2_op, tau_max, s_peak)

        g1 = Graph([s2_op], [tau_b_op, tau_f_op])
        s2_lst = np.linspace(-2*sR, 2*sR, 1000)
        tau_b_lst = []
        tau_f_lst = []
        tau_b_lst_ref = []
        tau_f_lst_ref = []
        for s2 in s2_lst:
            tau_b, tau_f = g1.forward(s2)
            tau_b_lst.append(tau_b)
            tau_f_lst.append(tau_f)
            tau_b_ref = bond_stress_ref(s2, tau_max, s_peak, sR)
            tau_b_lst_ref.append(tau_b_ref)
            tau_f_ref = friction_stress_ref(s2, tau_max, s_peak)
            tau_f_lst_ref.append(tau_f_ref)
        plt.subplot(2, 1, 1)
        plt.plot(s2_lst, tau_b_lst, 'r')
        plt.plot(s2_lst, tau_b_lst_ref, 'b')
        plt.subplot(2 ,1, 2)
        plt.plot(s2_lst, tau_f_lst, 'r')
        plt.plot(s2_lst, tau_f_lst_ref, 'b')
        fig = plt.gcf()
        fig_path = path.expanduser("~/Desktop/TestBSIM.stress.png")
        fig.savefig(fig_path)

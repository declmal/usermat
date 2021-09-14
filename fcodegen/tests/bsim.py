import unittest

import matplotlib.pyplot as plt

from .test_utils import register_test, validate_od_func
from ..op_def import OpDef as od
from ..utils.type_utils import One, Zero, MinusOne
from ..utils.math_utils import get_piecewise_linear_info
from ..graph import Graph

one = od.scalar(One)
zero = od.scalar(Zero)
minus_one = od.scalar(MinusOne)

def estimate_peak_bond_stress(fcp):
    tau_max_data = 1.163 * fcp ** 0.75
    return tau_max_data

def estimate_peak_bond_stress_slip(db):
    s_peak_data = 0.07 * db
    return s_peak_data

def estimate_clear_spacing(db, ratio=0.5):
    sR_data = ratio * db
    return sR_data

def polygen(func, x0, x1, nsegs=4):
    assert x0 < x1, "x0: {}, x1: {}".format(x0, x1)
    inc = (x1-x0) / nsegs
    xs = [x0+inc*i for i in range(1, nsegs)]
    pointlist = [(x, func(x)) for x in xs]
    return pointlist

@validate_od_func
def bearing_stress(tau_max, s_peak, sR, s2, nsegs1=4, nsegs2=4):
    """
    """
    assert sR.data > 1.6 * s_peak.data
    points = [
        (-sR.data/s_peak.data, 0),
        (-1.6, -0.6),
        (-1.5, -0.6),
        *polygen(
            lambda x: 0.6 - 0.36 * ((-x-1.5)/1.35)**4,
            -1.5, -0.15, nsegs=nsegs1),
        (-0.15, -1.6*0.15),
        (0, 0),
        (0.1, 3*0.1),
        *polygen(
            lambda x: 0.75 - 0.45 * ((x-1)/0.9)**4,
            0.1, 1, nsegs=nsegs2),
        (1, 0.75),
        (1.1, 0.75),
        (sR.data/s_peak.data, 0),
    ]
    end_tuples = [(0, points[0]), (0, points[-1])]
    point_tuples = [
        (points[i-1], points[i]) for i in range(1, len(points))
    ]
    scalar_datas = get_piecewise_linear_info(
        end_tuples, point_tuples, points)
    scalars = [od.scalar(scalar_data) for scalar_data in scalar_datas]
    mono = od.monomial(one, s2, one, s_peak, minus_one)
    piecewise = od.piecewiselinear(mono, *scalars)
    tau_b = od.monomial(one, tau_max, one, piecewise, one)
    return tau_b

@validate_od_func
def friction_stress(tau_max, s_peak, sR, s2, nsegs1=4, nsegs2=4):
    """ The
    """
    points = [
        (-1.5, -0.25),
        *polygen(
            lambda x: 0.25 - 0.15 * ((-x-1.5)/1.35)**4,
            -1.5, -0.15, nsegs=nsegs1),
        (-0.15, -0.1),
        (0, 0),
        (0.1, 0.1),
        *polygen(
            lambda x: 0.25 - 0.15 * ((x-1)/0.9)**4,
            0.1, 1, nsegs=nsegs2),
        (1, 0.25),
    ]
    end_tuples = [(0, points[0]), (0, points[-1])]
    point_tuples = [
        (points[i-1], points[i]) for i in range(1, len(points))
    ]
    scalar_datas = get_piecewise_linear_info(
        end_tuples, point_tuples, points)
    scalars = [od.scalar(scalar_data) for scalar_data in scalar_datas]
    mono = od.monomial(one, s2, one, s_peak, minus_one)
    piecewise = od.piecewiselinear(mono, *scalars)
    tau_f = od.monomial(one, tau_max, one, piecewise, one)
    return tau_f

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
        tau_max_data = estimate_peak_bond_stress(fcp)
        s_peak_data = estimate_peak_bond_stress_slip(db)
        sR_data = estimate_clear_spacing(db)
        tau_max = od.scalar(tau_max_data)
        s_peak = od.scalar(s_peak_data)
        sR = od.scalar(sR_data)
        s2 = od.var("var")

        tau_b = bearing_stress(tau_max, s_peak, sR, s2)
        tau_f = friction_stress(tau_max, s_peak, sR, s2)
        g1 = Graph([s2], [tau_b, tau_f])
        ret = g1.forward(0.2)


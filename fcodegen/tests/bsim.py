from .test_utils import register_test, validate_od_func
from ..op_def import OpDef as od
from ..utils.type_utils import One, Zero, MinusOne

one = od.scalar(One)
zero = od.scalar(Zero)
minus_one = od.scalar(MinusOne)

@validate_od_func
def bond_stress(
    tau_b, tau_f, rou_n, rou_bs, rou_fs, rou_bs, rou_bc, rou_fc):
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
        rou_n: fcodegen.op.base.Op
            A bond stress reduction factor that accounts for the opening of
            splitting cracks in concrete.
        rou_bs: fcodegen.op.base.Op
            A bearing stress reduction factor that accounts for the yielding
            of the bar in tension.
        rou_fs: fcodegen.op.base.Op
            A friction stress reduction factor that accounts for the yielding
            of the bar in tension.
        rou_bc: fcodegen.op.base.Op
            A bearing stress reduction factor that accounts for the
            loading-history-dependent bond deterioration.
        rou_fc: fcodegen.op.base.Op
            A friction stress reduction factor that accounts for the
            loading-history-dependent bond deterioration.

        Returns
        -------
        tau2: fcodegen.op.base.Op
            Bond stress.
    """
    mono1 = od.monomial(one, rou_bs, one, rou_bc, one, tau_b, one)
    mono2 = od.monomial(one, rou_fs, one, rou_fc, one, tau_f, one)
    poly = od.polynomial(zero, mono1, one, mono2, one)
    tau2 = od.monimial(one, rou_n, one, poly, one)
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
    """ The rotation of the bar around its longitudinal axis is restrained.

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

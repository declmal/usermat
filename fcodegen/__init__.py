import logging

from .utils.log_utils import log_init
from .op import base
from .op import zero_deps
from .op import unary
from .op import asserts
from .op import power
from .op import binary
from .op import cnds
from .op import monomial
from .op import polynomial
from .op import piecewise_linear
from .expr import exprs

log_init(level=logging.DEBUG)

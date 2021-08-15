import logging

from .log_utils import log_init
from .op import monomial
from .op import polynomial
from .op import cnds
from .op import binary
from .op import unary
from .op import power
from .op import asserts
from .op import zero_deps

log_init(level=logging.DEBUG)

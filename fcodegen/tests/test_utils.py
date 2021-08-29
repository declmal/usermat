import logging

import numpy as np

from ..op_def import OpDef as od

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
            s = "{}.{}".format(self.__class__.__name__, func.__name__)
            self.logger = logging.getLogger(s)
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

def random_array(shape, low=0.0, high=1.0, to_list=True):
    data = np.random.rand(*shape)
    data = low + (high-low)*data
    if to_list:
        data = data.tolist()
    return data

from .. base import Op

""" validate functions
"""
def num_valid_func(num_deps):
    def wrapper(*deps):
        assert len(deps) == num_deps, \
            "invalid deps number: {}, ".format(len(deps)) + \
            "expected: {}".format(num_deps)
        for dep in deps:
            assert isinstance(dep, Op), \
                "invalid type of dep: {}".format(type(dep))
    return wrapper

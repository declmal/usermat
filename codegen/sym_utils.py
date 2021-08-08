import mxnet as mx

def sym_rename(sym, name):
    op_name = sym.attr("op_name")
    attrs, deps = sym.list_attr(), sym.get_children()
    if op_name == "null":
        assert deps is None and len(attrs) == 0
        return mx.sym.var(name=name)
    _op = getattr(mx.symbol, op_name)
    return _op(*deps, **attrs, name=name)

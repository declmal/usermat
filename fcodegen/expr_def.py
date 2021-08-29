class ExprDef(object):
    supported_exprs = {}

    @staticmethod
    def register_expr(cls):
        def expr_func(*args, **kwargs):
            expr = cls(*args, **kwargs)
            return expr
        # set expr type
        expr_type = cls.__name__.lower()
        assert expr_type not in ExprDef.supported_exprs, \
            "expr_type: {} has been registered, ".format(expr_type) + \
            "supported_exprs: {}".format(ExprDef.supported_exprs)
        # update expr manager
        ExprDef.supported_exprs[expr_type] = cls
        # set up expr_def
        setattr(ExprDef, expr_type, expr_func)
        return cls

    @staticmethod
    def get_expr_cls(expr_type):
        assert expr_type in ExprDef.supported_exprs, \
            "please register Expr: {} first".format(expr_type)
        cls = ExprDef.supported_exprs[expr_type]
        return cls

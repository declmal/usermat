from typing import List, Dict

from codegen.base import supported_ops

def register_graph_ops(cls):
    def op_func(op_cls):
        def wrapper(*deps: "Op") -> "Op":
            equivs: List[str] = op_cls.op_equiv_func(deps)
            for equiv in equivs:
                if equiv in GraphDef.equiv_map:
                    equiv_op_id: int = GraphDef.equiv_map[equiv]
                    equiv_op: "Op" = GraphDef.id_map[equiv_op_id]
                    return equiv_op
            cid: int = GraphDef.current_id
            for equiv in equivs:
                GraphDef.equiv_map[equiv] = cid
            op: "Op" = op_cls(*deps)
            op.set_id(cid)
            GraphDef.id_map[cid] = op
            GraphDef.current_id = cid + 1
            return op
        return wrapper

    for op_cls in supported_ops.values():
        op_type = getattr(op_cls, "op_type")
        setattr(cls, op_type, op_func(op_cls))
    return cls

@register_graph_ops
class GraphDef(object):
    current_id: int = 0
    equiv_map: Dict[str, int] = {}
    id_map: Dict[int, 'Op'] = {}

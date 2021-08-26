from .type_utils import FloatTypes, PositiveInf, NegativeInf

def validate_scalar(x):
    assert isinstance(x, FloatTypes) and \
        x not in [PositiveInf, NegativeInf], x

def validate_point(point):
    assert isinstance(point, tuple) and len(point) == 2, point
    x, y = point
    validate_scalar(x)
    validate_scalar(y)

def get_bias(point, k):
    validate_point(point)
    validate_scalar(k)
    x, y = point
    b = y - k*x
    return b

def get_segment_info(point, point1):
    validate_point(point)
    validate_point(point1)
    x, y = point
    x1, y1 = point1
    assert x != x1, (point, point1)
    k = (y1-y) / (x1-x)
    b = get_bias(point, k)
    return k, b

def get_piecewise_linear_info(end_tuples, point_tuples, points):
    assert len(end_tuples) == 2, end_tuples
    assert len(points) > 0,  points
    assert len(point_tuples) == len(points) - 1, point_tuples
    for k, p in end_tuples:
        validate_scalar(k)
        validate_point(p)
    for p, p1 in point_tuples:
        validate_point(p)
        validate_point(p1)
    for p in points:
        validate_point(p)
    ret = []
    k, p = end_tuples[0]
    b = get_bias(p, k)
    ret.append(k)
    ret.append(b)
    spt, c = points[0]
    ret.append(spt)
    ret.append(c)
    for i in range(len(point_tuples)):
        p, p1 = point_tuples[i]
        k, b = get_segment_info(p, p1)
        ret.append(k)
        ret.append(b)
        spt, c = points[i+1]
        ret.append(spt)
        ret.append(c)
    k, p = end_tuples[1]
    b = get_bias(p, k)
    ret.append(k)
    ret.append(b)
    return ret

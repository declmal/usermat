from .type_utils import FloatTypes, PositiveInf, NegativeInf, Half, Zero

def validate_scalar_data(data):
    assert isinstance(data, FloatTypes), \
        "invalid type of data: {}".format(type(data))
    assert data not in [PositiveInf, NegativeInf], \
        "invalid value of data: {}".format(data)

def validate_pl_scalar_datas(*datas):
    num_data = len(datas)
    assert num_data >= 6 and (num_data-2) % 4 == 0, num_data
    splits = []
    for i, data in enumerate(datas):
        validate_scalar_data(data)
        if (i-2) % 4 == 0:
            if splits:
                pdata = splits[-1]
                assert data > pdata, \
                    "invalid split points, data: {}, i: {}".format(data, i)
            splits.append(data)

def validate_point(point):
    assert isinstance(point, tuple) and len(point) == 2, point
    x, y = point
    validate_scalar_data(x)
    validate_scalar_data(y)

def get_bias(point, k):
    validate_point(point)
    validate_scalar_data(k)
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
        validate_scalar_data(k)
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

def get_piecewise_linear_diff_info(*datas):
    validate_pl_scalar_datas(*datas)
    ndatas = []
    for i in range(0, len(datas)-2, 4):
        k, b, spt, c, k1, b1 = datas[i:i+6]
        ndatas.append(Zero)
        ndatas.append(k)
        ndatas.append(spt)
        dst = abs(k*spt+b-c)
        dst1 = abs(k1*spt+b1-c)
        if dst < dst1:
            nc = k
        elif dst > dst1:
            nc = k1
        else:
            nc = Half * (k+k1)
        ndatas.append(nc)
    k = datas[-2]
    ndatas.append(Zero)
    ndatas.append(k)
    return ndatas

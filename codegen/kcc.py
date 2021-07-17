import numpy as np
import matplotlib.pyplot as plt


class Function(object):
    def forward(self, *args):
        return self._forward(*args)

class PiecewiseLinear2D(Function):
    def __init__(self, 2d_points):
        assert isinstance(2d_points, np.ndarray), \
            "invalid type of 2d_points: {}".format(type(2d_points))
        shp = 2d_points.shape
        assert len(shp) == 2 and shp[1] == 2, \
            "invalid shape of 2d_points: {}".format(shp)
        for i in range(shp[0]-1):
            assert 2d_points[i][0] < 2d_points[i+1][0], \
                "unsorted data: {}".format(2d_points)

# if __name__ == "__main__":
    # plt.figure()
    # plt.plot(x, y, 'o')
    # plt.plot(x_hat, y_hat, '-')
    # plt.show()



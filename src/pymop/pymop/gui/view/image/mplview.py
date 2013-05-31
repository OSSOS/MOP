__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas
from stsci import numdisplay


class MPLImageViewer(object):
    """
    Display images using matplotlib.
    """

    def __init__(self, parent):
        # Create the actual mpl figure we will draw on
        self.figure = plt.figure()

        self.axes = self.figure.add_subplot(1, 1, 1)
        self.axes.set_aspect("equal", adjustable="datalim")
        self.axes.autoscale_view(tight=True, scalex=True, scaley=True)

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

    def view_image(self, hdulist):
        plt.imshow(zscale(hdulist[0].data), cmap="gray")

    def draw_circle(self, x, y, radius):
        source_circle = plt.Circle((x, y), radius, color="y", fill=False)
        self.axes.add_patch(source_circle)

    def close(self):
        pass


def zscale(img):
    """
    Performs the zscale operation on an image.

    Args:
      img: numpy.ndarray
        The image to be scaled.

    Returns:
      scaled_img: numpy.ndarray
        The input image after scaling.
    """
    # Using the default values, but listing explicitly
    z1, z2 = numdisplay.zscale.zscale(img, nsamples=1000, contrast=0.25)
    return normalize(img, z1, z2)


def normalize(arr, lower, upper):
    """
    Normalizes a numpy array to values between the given lower and upper
    bounds.
    """
    scaling_factor = float(upper - lower) / (arr.max() - arr.min())
    return scaling_factor * arr + lower

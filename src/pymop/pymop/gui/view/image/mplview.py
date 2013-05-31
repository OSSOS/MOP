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

        self.source_circle = None

    def view_image(self, hdulist):
        plt.imshow(zscale(hdulist[0].data), cmap="gray")

    def draw_circle(self, x, y, radius):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        if self.source_circle is not None:
            self.source_circle.remove()

        circle = plt.Circle((x, y), radius, color="y", fill=False)
        self.axes.add_patch(circle)
        self.source_circle = DraggableCircle(circle)
        self.source_circle.connect()

    def close(self):
        pass


class DraggableCircle(object):
    """
    A circle to be displayed in Matplotlib which can be dragged around
    within the figure.

    Very useful reference:
    http://matplotlib.org/users/event_handling.html
    """

    def __init__(self, circle):
        """
        Constructor.

        Args:
          circle: matplotlib Circle patch
        """
        self.circle = circle
        self.press = None

    def connect(self):
        """
        Connect to start listening for the relevant events.
        """
        self.cidpress = self.circle.figure.canvas.mpl_connect(
            "button_press_event", self.on_press)
        self.cidrelease = self.circle.figure.canvas.mpl_connect(
            "button_release_event", self.on_release)
        self.cidmotion = self.circle.figure.canvas.mpl_connect(
            "motion_notify_event", self.on_motion)

    def on_press(self, event):
        if event.inaxes != self.circle.axes:
            return

        contains_event, attrs = self.circle.contains(event)
        print attrs

        if not contains_event:
            return

        x0, y0 = self.circle.center

        self.press = x0, y0, event.xdata, event.ydata

    def on_motion(self, event):
        if self.press is None or event.inaxes != self.circle.axes:
            return

        x0, y0, xpress, ypress = self.press

        dx = event.xdata - xpress
        dy = event.ydata - ypress

        self.circle.center = (x0 + dx, y0 + dy)

        self.circle.figure.canvas.draw()

    def on_release(self, event):
        self.press = None
        self.circle.figure.canvas.draw()

    def remove(self):
        """Disconnects all the stored connection ids"""
        self.circle.figure.canvas.mpl_disconnect(self.cidpress)
        self.circle.figure.canvas.mpl_disconnect(self.cidrelease)
        self.circle.figure.canvas.mpl_disconnect(self.cidmotion)

        self.circle.remove()


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

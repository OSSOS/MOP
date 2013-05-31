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

        self.interaction_context = InteractionContext(self.figure, self.axes)

    def view_image(self, hdulist):
        plt.imshow(zscale(hdulist[0].data), cmap="gray")

    def draw_circle(self, x, y, radius):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        self.interaction_context.create_circle(x, y, radius)

    def close(self):
        pass


class InteractionContext(object):
    """
    Very useful reference for matplotlib event handling:
    http://matplotlib.org/users/event_handling.html
    """

    def __init__(self, figure, axes):
        self.figure = figure
        self.axes = axes

        self._connect()
        self.state = MoveCircleState()

        self.circle = None

    def create_circle(self, x, y, radius):
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="y", fill=False)
        self.axes.add_patch(self.circle)

    def _connect(self):
        """
        Connect to start listening for the relevant events.
        """
        self.cidpress = self.figure.canvas.mpl_connect(
            "button_press_event", self.on_press)
        self.cidrelease = self.figure.canvas.mpl_connect(
            "button_release_event", self.on_release)
        self.cidmotion = self.figure.canvas.mpl_connect(
            "motion_notify_event", self.on_motion)

    def on_press(self, event):
        self.state.on_press(self, event)

    def on_motion(self, event):
        self.state.on_motion(self, event)

    def on_release(self, event):
        self.state.on_release(self, event)

    def _disconnect(self):
        """Disconnects all the stored connection ids"""
        self.figure.canvas.mpl_disconnect(self.cidpress)
        self.figure.figure.canvas.mpl_disconnect(self.cidrelease)
        self.figure.figure.canvas.mpl_disconnect(self.cidmotion)


class MoveCircleState(object):
    def __init__(self):
        self.press = None

    def on_press(self, context, event):
        if event.inaxes != context.circle.axes:
            return

        contains_event, _ = context.circle.contains(event)

        if not contains_event:
            return

        circle_x, circle_y = context.circle.center

        self.press = circle_x, circle_y, event.xdata, event.ydata

    def on_motion(self, context, event):
        if self.press is None or event.inaxes != context.circle.axes:
            return

        circle_x, circle_y, mouse_x, mouse_y = self.press

        dx = event.xdata - mouse_x
        dy = event.ydata - mouse_y

        context.circle.center = (circle_x + dx, circle_y + dy)

        context.circle.figure.canvas.draw()

    def on_release(self, context, event):
        self.press = None
        context.circle.figure.canvas.draw()


class CreateCircleState(object):
    def __init__(self):
        self.press = None

    def on_press(self, context, event):
        pass

    def on_motion(self, context, event):
        pass

    def on_release(self, context, event):
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

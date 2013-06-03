__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas
from stsci import numdisplay


class MPLViewerError(object):
    """Base exception for matplotlib viewer"""


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

        self.current_image = None

    def view_image(self, fits_image):
        plt.imshow(zscale(fits_image.as_hdulist()[0].data), cmap="gray")
        self.current_image = fits_image

    def draw_circle(self, x, y, radius):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        self.interaction_context.create_circle(x, y, radius)

    def close(self):
        self.interaction_context.disconnect()


class InteractionContext(object):
    """
    Very useful reference for matplotlib event handling:
    http://matplotlib.org/users/event_handling.html
    """

    def __init__(self, figure, axes):
        self.figure = figure
        self.axes = axes

        self._connect()

        self.circle = None
        self.state = CreateCircleState(self)

    def create_circle(self, x, y, radius):
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="y", fill=False)
        self.axes.add_patch(self.circle)

        self.redraw()

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
        if event.inaxes != self.axes:
            return

        if self.circle is None:
            in_circle = False
        else:
            in_circle, _ = self.circle.contains(event)

        if in_circle:
            self.state = MoveCircleState(self)
        else:
            self.state = CreateCircleState(self)

        self.state.on_press(event)

    def on_motion(self, event):
        if event.inaxes != self.axes:
            return

        self.state.on_motion(event)
        self.redraw()

    def on_release(self, event):
        self.state.on_release(event)
        self.redraw()

    def redraw(self):
        self.figure.canvas.draw()

    def disconnect(self):
        """Disconnects all the stored connection ids"""
        self.figure.canvas.mpl_disconnect(self.cidpress)
        self.figure.figure.canvas.mpl_disconnect(self.cidrelease)
        self.figure.figure.canvas.mpl_disconnect(self.cidmotion)


class MoveCircleState(object):
    def __init__(self, context):
        if context.circle is None:
            raise MPLImageViewer("Can not move a circle if it doesn't exist!")

        self.context = context

        self.pressed = False
        self.center_x = None
        self.center_y = None
        self.mouse_x = None
        self.mouse_y = None

    def on_press(self, event):
        self.pressed = True

        self.center_x, self.center_y = self.context.circle.center
        self.mouse_x = event.xdata
        self.mouse_y = event.ydata

    def on_motion(self, event):
        if not self.pressed:
            return

        dx = event.xdata - self.mouse_x
        dy = event.ydata - self.mouse_y

        # Update the circle's center
        self.context.circle.center = (self.center_x + dx, self.center_y + dy)

    def on_release(self, event):
        self.pressed = False
        self.center_x = None
        self.center_y = None
        self.mouse_x = None
        self.mouse_y = None


class CreateCircleState(object):
    def __init__(self, context):
        self.context = context

        self.pressed = False
        self.startx = None
        self.starty = None
        self.endx = None
        self.endy = None

    def on_press(self, event):
        self.pressed = True

        self.startx = event.xdata
        self.starty = event.ydata

    def on_motion(self, event):
        if not self.pressed:
            return

        self.endx = event.xdata
        self.endy = event.ydata

        centerx = float(self.startx + self.endx) / 2
        centery = float(self.starty + self.endy) / 2

        radius = max(abs(self.startx - self.endx) / 2,
                     abs(self.starty - self.endy) / 2)

        self.context.create_circle(centerx, centery, radius)

    def on_release(self, event):
        self.pressed = False
        self.startx = None
        self.starty = None
        self.endx = None
        self.endy = None


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

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas
from stsci import numdisplay

from pymop import config


class MPLViewerError(object):
    """Base exception for matplotlib viewer"""


class MPLImageViewer(object):
    """
    Display images using matplotlib.
    """

    def __init__(self, parent):
        # Create the actual mpl figure we will draw on
        self.figure = plt.figure()

        # Don't draw extra whitespace around image
        self.axes = plt.Axes(self.figure, [0., 0., 1., 1.])

        self.axes.set_aspect("equal", adjustable="datalim")

        # Make the axes fit the image tightly
        imgwidth = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")
        imgheight = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        self.axes.set_xlim([0, imgwidth])
        self.axes.set_ylim([0, imgheight])

        # Don't draw tick marks and labels
        self.axes.set_axis_off()

        self.figure.add_axes(self.axes)

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

        self.interaction_context = InteractionContext(self.figure, self.axes)

        self.current_image = None

    def view_image(self, fits_image):
        plt.imshow(zscale(fits_image.as_hdulist()[0].data), cmap="gray")
        self.current_image = fits_image

    def has_had_interaction(self):
        return self.interaction_context.has_had_interaction()

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

    MOUSE_BUTTON_LEFT = 1
    MOUSE_BUTTON_RIGHT = 3

    def __init__(self, figure, axes):
        self.figure = figure
        self.axes = axes

        self._connect()

        self.circle = None
        self.state = CreateCircleState(self)

        self._has_had_interaction = False

    def has_had_interaction(self):
        return self._has_had_interaction

    def create_circle(self, x, y, radius):
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="y", fill=False)
        self.axes.add_patch(self.circle)

        self.redraw()

    def update_circle(self, x, y, radius=None):
        if self.circle is None:
            if radius is None:
                raise MPLViewerError("No circle to update.")
            else:
                # For convenience go ahead and make one
                self.create_circle(x, y, radius)

        self.circle.center = (x, y)

        if radius is not None:
            self.circle.radius = radius

        self._has_had_interaction = True

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

        if event.button == InteractionContext.MOUSE_BUTTON_LEFT:
            self.state = self._choose_left_click_state(event)
        elif event.button == InteractionContext.MOUSE_BUTTON_RIGHT:
            self.state = AdjustColormapState(self)
        else:
            # Ignore any other button such as middle click.
            return

        self.state.on_press(event)

    def _choose_left_click_state(self, event):
        if self.circle is None:
            in_circle = False
        else:
            in_circle, _ = self.circle.contains(event)

        if in_circle:
            return MoveCircleState(self)
        else:
            return CreateCircleState(self)

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

        self.context.update_circle(self.center_x + dx, self.center_y + dy)

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

        self.context.update_circle(centerx, centery, radius)

    def on_release(self, event):
        self.pressed = False
        self.startx = None
        self.starty = None
        self.endx = None
        self.endy = None


class AdjustColormapState(object):
    """
    TODO: refactor out similarity of these states
    """

    def __init__(self, context):
        self.context = context

        self.pressed = False

    def on_press(self, event):
        self.pressed = True
        print "TODO: set up colormap adjustment"

    def on_motion(self, event):
        if not self.pressed:
            return

        print "TODO: perform colormap adjustment"

    def on_release(self, event):
        self.pressed = False
        print "TODO: colormap adjustment finished"


class GrayscaleColorMap(object):
    """
    An image color map, which allows its contrast and bias to be controlled.

    Refer to http://matplotlib.org/api/colors_api.html,
    specifically class matplotlib.colors.LinearSegmentedColormap
    to better understand how this works.

    For an example, also see
    http://matplotlib.org/examples/pylab_examples/custom_cmap.html?highlight=codex%20colormap

    For explanation of contrast and bias as in ds9, see:
    http://chandra-ed.harvard.edu/learning_ds9_page2.html
    """

    def __init__(self):
        self.x_spread = 1.0
        self.y_spread = 1.0

        self.x_offset = 0.0
        self._last_bias = 0.5

        self._build_cdict()

    def _build_cdict(self):
        lower_segment_x = self._clip(self.x_offset + 0.5 - self.x_spread / 2)
        upper_segment_x = self._clip(self.x_offset + 0.5 + self.x_spread / 2)

        min_y = self._clip(0.5 - (self.y_spread / 2))
        max_y = self._clip(0.5 + (self.y_spread / 2))

        self.min_bounds = (0.0, min_y, min_y)
        self.lower_segment_bounds = (lower_segment_x, min_y, min_y)
        self.upper_segment_bounds = (upper_segment_x, max_y, max_y)
        self.max_bounds = (1.0, max_y, max_y)

        self.cdict = {}
        for color in ["red", "green", "blue"]:
            self.cdict[color] = [self.min_bounds, self.lower_segment_bounds,
                                 self.upper_segment_bounds, self.max_bounds]

    def set_bias(self, bias):
        """
        Adjusts the image bias.

        Bias determines where the color changes start.  At low bias, low
        intensities (i.e., low pixel values) will have non-zero color
        differences, while at high bias only high pixel values will have
        non-zero differences

        Args:
          bias: float
            A number between 0 and 1.  Note that upon initialization the
            colormap has a default bias of 0.5.

        Returns: void
        """
        self.x_offset += (self._last_bias - bias)
        self._last_bias = bias

        self._build_cdict()

    def set_contrast(self, contrast):
        """
        Adjusts the image contrast.

        Contrast refers to the rate of change of color with color level.
        At low contrast, color changes gradually over many intensity
        levels, while at high contrast it can change rapidly within a
        few levels

        Args:
          contrast: float
            A number between 0 and 1.  Note that upon initialization the
            colormap has a default contrast value of 0.5.

        Returns: void
        """
        self.x_spread = 2 * contrast
        self.y_spread = 2.0 - 2 * contrast

        self._build_cdict()

    def _clip(self, value):
        """Clip to range 0 to 1"""
        return clip(value, 0, 1)

    def as_mpl_cmap(self):
        return LinearSegmentedColormap("Custom grayscale", self.cdict)


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


def clip(value, min_val, max_val):
    """
    Clip a value to a certain range.

    Args:
      value: number
        The value to be clipped.
      min_val: number
        The lowest allowable value.
      max_val: number
        The maximum allowable value.

    Returns:
      clipped: number
        The original value if it is between min_val and max_val.
        If it is less than min_val, return min_val.  If it is greater
        than max_val, return max_val.

    Examples;
      clip(0.1, 0, 1) -> 0.1
      clip(-0.1, 0, 1) -> 0
    """
    return max(min(value, max_val), min_val)

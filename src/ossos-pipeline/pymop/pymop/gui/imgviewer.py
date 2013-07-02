__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np
import wx
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas
from stsci import numdisplay

from pymop.gui import config


class MPLViewerError(object):
    """Base exception for matplotlib viewer"""


class ColormappedFitsImage(object):
    def __init__(self, fits_image):
        self.fits_image = fits_image
        self.colormap = GrayscaleColorMap()

    def update_contrast(self, contrast_diff):
        self.colormap.update_contrast(contrast_diff)

    def update_bias(self, bias_diff):
        self.colormap.update_bias(bias_diff)

    def reset_colormap(self):
        self.colormap.set_defaults()

    def get_image_data(self):
        return self.fits_image.as_hdulist()[0].data

    def get_cmap(self):
        return self.colormap.as_mpl_cmap()


class MPLImageViewer(object):
    """
    Display images using matplotlib.
    """

    def __init__(self, parent):
        # Create the actual mpl figure we will draw on
        self.figure = plt.figure()

        # Don't draw extra whitespace around image
        self.colorbar_height_portion = 0.05
        self.axes = plt.Axes(self.figure, [0.0, self.colorbar_height_portion,
                                           1., 1.])

        self.axes.set_aspect("equal", adjustable="datalim")

        # Make the axes fit the image tightly
        self.imgwidth = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")
        self.imgheight = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        self.axes.set_xlim([0, self.imgwidth])
        self.axes.set_ylim([0, self.imgheight])

        # Don't draw tick marks and labels
        self.axes.set_axis_off()

        self.figure.add_axes(self.axes)

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

        self.interaction_context = InteractionContext(self)

        self.current_image = None
        self.axes_image = None
        self.colorbar = None

        self.circle = None

        self._has_had_interaction = False

        self._viewed_images = {}

    def view_image(self, fits_image):
        # TODO: defaultdict?
        if fits_image not in self._viewed_images:
            colormapped_image = ColormappedFitsImage(fits_image)
            self._viewed_images[fits_image] = colormapped_image

        self.current_image = self._viewed_images[fits_image]

        processed_image_data = zscale(self.current_image.get_image_data())

        if self.axes_image is None:
            self.axes_image = plt.imshow(processed_image_data,
                                         cmap=self.current_image.get_cmap())
        else:
            # We re-use the old AxesImage object so that the colorbar can
            # be conveniently updated.  The colorbar gets left in a disconnected
            # state if we call imshow again.
            self.axes_image.set_data(processed_image_data)
            self.axes_image.set_clim(vmin=np.min(processed_image_data),
                                     vmax=np.max(processed_image_data))
            self._refresh_displayed_colormap()
            plt.draw()

        if self.colorbar is None:
            # Create axes for colorbar.  Make it tightly fit the image.
            divider = make_axes_locatable(self.axes)
            size = str(100 * self.colorbar_height_portion) + "%"
            self.cax = divider.append_axes("bottom", size=size, pad=0.05)
            self.colorbar = self.figure.colorbar(
                self.axes_image, orientation="horizontal", cax=self.cax)

    def _refresh_displayed_colormap(self):
        self.axes_image.set_cmap(self.current_image.get_cmap())
        self.axes_image.changed()
        self.redraw()

    def update_colormap(self, dx, dy):
        assert self.current_image is not None, "No image to update colormap for."

        contrast_diff = float(-dy) / self.imgheight
        bias_diff = float(dx) / self.imgwidth

        self.current_image.update_contrast(contrast_diff)
        self.current_image.update_bias(bias_diff)

        self._refresh_displayed_colormap()

    def reset_colormap(self):
        self.current_image.reset_colormap()
        self._refresh_displayed_colormap()

    def has_had_interaction(self):
        return self._has_had_interaction

    def draw_circle(self, x, y, radius):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="b", fill=False)
        self.axes.add_patch(self.circle)

        self.redraw()

    def update_circle(self, x, y, radius=None):
        if self.circle is None:
            if radius is None:
                raise MPLViewerError("No circle to update.")
            else:
                # For convenience go ahead and make one
                self.draw_circle(x, y, radius)

        self.circle.center = (x, y)

        if radius is not None:
            self.circle.radius = radius

        self._has_had_interaction = True

        self.redraw()

    def get_circle(self):
        return self.circle

    def redraw(self):
        self.figure.canvas.draw()

    def is_event_in_axes(self, event):
        return self.axes == event.inaxes

    def close(self):
        self.interaction_context.disconnect()

    def register_event_handler(self, eventname, handler):
        return self.figure.canvas.mpl_connect(eventname, handler)

    def deregister_event_handler(self, id):
        self.figure.canvas.mpl_disconnect(id)


class InteractionContext(object):
    """
    Very useful reference for matplotlib event handling:
    http://matplotlib.org/users/event_handling.html
    """

    MOUSE_BUTTON_LEFT = 1
    MOUSE_BUTTON_RIGHT = 3

    def __init__(self, viewer):
        self.viewer = viewer
        self._register_event_handlers()

        self.state = CreateCircleState(self)

    def _register_event_handlers(self):
        """
        Connect to start listening for the relevant events.
        """
        self.cidpress = self.viewer.register_event_handler(
            "button_press_event", self.on_press)
        self.cidrelease = self.viewer.register_event_handler(
            "button_release_event", self.on_release)
        self.cidmotion = self.viewer.register_event_handler(
            "motion_notify_event", self.on_motion)

    def on_press(self, event):
        if not self.viewer.is_event_in_axes(event):
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
        circle = self.viewer.get_circle()

        if circle is None:
            in_circle = False
        else:
            in_circle, _ = circle.contains(event)

        if in_circle:
            return MoveCircleState(self)
        else:
            return CreateCircleState(self)

    def on_motion(self, event):
        if not self.viewer.is_event_in_axes(event):
            return

        self.state.on_motion(event)
        self.viewer.redraw()

    def on_release(self, event):
        self.state.on_release(event)
        self.viewer.redraw()

    def get_circle(self):
        return self.viewer.get_circle()

    def update_circle(self, x, y, radius=None):
        self.viewer.update_circle(x, y, radius)

    def update_colormap(self, dx, dy):
        self.viewer.update_colormap(dx, dy)

    def disconnect(self):
        """Disconnects all the stored connection ids"""
        self.viewer.deregister_event_handler(self.cidpress)
        self.viewer.deregister_event_handler(self.cidrelease)
        self.viewer.deregister_event_handler(self.cidmotion)


class BaseInteractionState(object):
    def __init__(self, context):
        self.context = context
        self._set_blank_state()

    def _set_blank_state(self):
        self.pressed = False
        self.had_drag = False

        self.start_x = None
        self.start_y = None
        self.last_x = None
        self.last_y = None

    def on_press(self, event):
        self.pressed = True

        self.start_x = event.xdata
        self.start_y = event.ydata
        self.last_x = self.start_x
        self.last_y = self.start_y

    def on_motion(self, event):
        if not self.pressed:
            return

        self.had_drag = True
        self.on_drag(event)

        self.last_x = event.xdata
        self.last_y = event.ydata

    def on_drag(self, event):
        """
        Implement to provide state-specific behaviour on motion.
        """
        pass

    def on_release(self, event):
        self._set_blank_state()


class RecenteringState(BaseInteractionState):
    def on_release(self, event):
        if (self.pressed and
                not self.had_drag and
                    self.context.get_circle() is not None):
            self.context.update_circle(self.start_x, self.start_y)

        super(RecenteringState, self).on_release(event)


class MoveCircleState(RecenteringState):
    def __init__(self, context):
        super(MoveCircleState, self).__init__(context)

        if context.get_circle() is None:
            raise MPLImageViewer("Can not move a circle if it doesn't exist!")

    def on_drag(self, event):
        center_x, center_y = self.context.get_circle().center

        dx = event.xdata - self.last_x
        dy = event.ydata - self.last_y

        self.context.update_circle(center_x + dx, center_y + dy)


class CreateCircleState(RecenteringState):
    def __init__(self, context):
        super(CreateCircleState, self).__init__(context)

    def on_drag(self, event):
        center_x = float(self.start_x + event.xdata) / 2
        center_y = float(self.start_y + event.ydata) / 2

        radius = max(abs(self.start_x - event.xdata) / 2,
                     abs(self.start_y - event.ydata) / 2)

        self.context.update_circle(center_x, center_y, radius)


class AdjustColormapState(BaseInteractionState):
    def __init__(self, context):
        super(AdjustColormapState, self).__init__(context)

    def on_drag(self, event):
        self.context.update_colormap(event.xdata - self.last_x,
                                     event.ydata - self.last_y)


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
        self.set_defaults()

    def _build_cdict(self):
        lower_segment_x = self._clip(self.x_offset + 0.5 - self.x_spread / 2)
        upper_segment_x = self._clip(self.x_offset + 0.5 + self.x_spread / 2)

        max_y = self._clip(0.5 + (self.y_spread / 2))
        min_y = self._clip(0.5 - (self.y_spread / 2))

        if lower_segment_x > upper_segment_x:
            # If they try to go past maximum contrast, flip the colorbar
            # ds9 style
            lower_segment_x, upper_segment_x = upper_segment_x, lower_segment_x
            min_y, max_y = max_y, min_y

        # NOTE: max_y used at lower bounds and min_y used at upper bounds in
        # order to invert the image's colourmap by default.
        self.min_bounds = (0.0, max_y, max_y)
        self.lower_segment_bounds = (lower_segment_x, max_y, max_y)
        self.upper_segment_bounds = (upper_segment_x, min_y, min_y)
        self.max_bounds = (1.0, min_y, min_y)

        self.cdict = {}
        for color in ["red", "green", "blue"]:
            self.cdict[color] = [self.min_bounds, self.lower_segment_bounds,
                                 self.upper_segment_bounds, self.max_bounds]

    def set_defaults(self):
        self._contrast = 0.5
        self._bias = 0.5

        self.x_spread = 1.0
        self.y_spread = 1.0
        self.x_offset = 0.0

        self._build_cdict()

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
        self.x_offset += (bias - self._bias)
        self._bias = bias

        self._build_cdict()

    def update_bias(self, bias_diff):
        self.set_bias(self._bias + bias_diff)

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
        self._contrast = contrast

        self.x_spread = 2 * (1.0 - contrast)
        self.y_spread = 2.0 - 2 * (1.0 - contrast)

        self._build_cdict()

    def update_contrast(self, contrast_diff):
        self.set_contrast(self._contrast + contrast_diff)

    def _clip(self, value):
        """Clip to range 0 to 1"""
        return clip(value, 0, 1)

    def as_mpl_cmap(self):
        return LinearSegmentedColormap("CustomGrayscale", self.cdict)


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
    return np.clip(img, z1, z2)


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

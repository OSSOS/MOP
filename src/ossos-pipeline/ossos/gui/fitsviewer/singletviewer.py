__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np
import wx
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas
from stsci import numdisplay

from ossos.gui import config
from ossos.gui.fitsviewer.colormap import ColormappedFitsImage
from ossos.gui.fitsviewer.exceptions import MPLViewerError
from ossos.gui.fitsviewer.interaction import InteractionContext


class MPLFitsImageViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent):
        self._parent = parent

        # Create the actual mpl figure we will draw on
        self.figure = plt.figure()

        self.colorbar_height_portion = 0.05

        # limits specified as [left, bottom, width, height]
        # leave 2.5% border on left and right
        self.axes = plt.Axes(self.figure, [0.025, 0.0, 0.95, 1.0])

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

        self.xy_changed = Signal()

    def view_image(self, fits_image, redraw=True):
        if fits_image not in self._viewed_images:
            colormapped_image = ColormappedFitsImage(fits_image)
            self._viewed_images[fits_image] = colormapped_image

        self.current_image = self._viewed_images[fits_image]

        processed_image_data = zscale(self.current_image.get_image_data())

        if self.axes_image is None:
            self.axes_image = plt.imshow(processed_image_data,
                                         extent=(1,self.imgwidth+1,self.imgheight+1,1),
                                         cmap=self.current_image.get_cmap())
        else:
            # We re-use the old AxesImage object so that the colorbar can
            # be conveniently updated.  The colorbar gets left in a disconnected
            # state if we call imshow again.
            self.axes_image.set_data(processed_image_data)
            self.axes_image.set_clim(vmin=np.min(processed_image_data),
                                     vmax=np.max(processed_image_data))
            self._refresh_displayed_colormap()

        if self.colorbar is None:
            # Create axes for colorbar.  Make it tightly fit the image.
            divider = make_axes_locatable(self.axes)
            size = str(100 * self.colorbar_height_portion) + "%"
            self.cax = divider.append_axes("bottom", size=size, pad=0.05)
            self.colorbar = self.figure.colorbar(
                self.axes_image, orientation="horizontal", cax=self.cax)

        if redraw:
            self.redraw()

    def _refresh_displayed_colormap(self):
        self.axes_image.set_cmap(self.current_image.get_cmap())
        self.axes_image.changed()

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

    def draw_circle(self, x, y, radius, redraw=True):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="b", fill=False)
        self.axes.add_patch(self.circle)

        if redraw:
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

        self.xy_changed.fire(x, y)

    def get_circle(self):
        return self.circle

    def redraw(self):
        self.figure.canvas.draw()

    def release_focus(self):
        self._parent.SetFocus()

    def is_event_in_axes(self, event):
        return self.axes == event.inaxes

    def close(self):
        self.interaction_context.disconnect()

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

    def register_mpl_event_handler(self, eventname, handler):
        return self.figure.canvas.mpl_connect(eventname, handler)

    def deregister_mpl_event_handler(self, id_):
        self.figure.canvas.mpl_disconnect(id_)

    def as_widget(self):
        return self.canvas


class Signal(object):
    def __init__(self):
        self._handlers = []

    def connect(self, handler):
        self._handlers.append(handler)

    def fire(self, *args):
        for handler in self._handlers:
            handler(*args)


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

__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np

import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from stsci import numdisplay

from ossos.gui.fitsviewer.colormap import GrayscaleColorMap
from ossos.gui.fitsviewer.exceptions import MPLViewerError
from ossos.gui.fitsviewer.interaction import InteractionContext, Signal


class DisplayableImageSinglet(object):
    def __init__(self, hdulist):
        """
        Args:
          hdulist: astropy.io.fits.HDUList
            The FITS image to be displayed.
        """
        self.hdulist = hdulist
        self.figure = None
        self.axes = None

        self.circle = None

        self.display_changed = Signal()
        self.xy_changed = Signal()
        self.focus_released = Signal()

        self._colormap = GrayscaleColorMap()

        self._mpl_event_handlers = {}
        self._interaction_context = None

    @property
    def image_data(self):
        return self.hdulist[0].data

    @property
    def image_width(self):
        return self.image_data.shape[0]

    @property
    def image_height(self):
        return self.image_data.shape[1]

    def render(self, canvas=None):
        if self.figure is None:
            self._do_render()

        if canvas is None:
            plt.show()
        else:
            canvas.figure = self.figure

            parent_size = canvas.GetClientSize()

            figure_dpi = self.figure.get_dpi()
            self.figure.set_size_inches(parent_size[0] / figure_dpi,
                                        parent_size[1] / figure_dpi)

            self._apply_event_handlers(canvas)

    def update_colormap(self, dx, dy):
        contrast_diff = float(-dy) / self.image_height
        bias_diff = float(dx) / self.image_width

        self._colormap.update_contrast(contrast_diff)
        self._colormap.update_bias(bias_diff)

        self._refresh_displayed_colormap()

    def reset_colormap(self):
        self._colormap.set_defaults()
        self._refresh_displayed_colormap()

    def draw_circle(self, x, y, radius):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        if self.circle is not None:
            self.circle.remove()

        self.circle = plt.Circle((x, y), radius, color="b", fill=False)
        self.axes.add_patch(self.circle)

        self.display_changed.fire()

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

        self.xy_changed.fire(x, y)
        self.display_changed.fire()

    def is_event_in_axes(self, event):
        return self.axes == event.inaxes

    def register_mpl_event_handler(self, eventname, handler):
        handler_id = self.figure.canvas.mpl_connect(eventname, handler)
        self._mpl_event_handlers[handler_id] = (eventname, handler)
        return handler_id

    def deregister_mpl_event_handler(self, id_):
        self.figure.canvas.mpl_disconnect(id_)
        del self._mpl_event_handlers[id_]

    def _apply_event_handlers(self, canvas):
        for eventname, handler in self._mpl_event_handlers.itervalues():
            canvas.mpl_connect(eventname, handler)

    def release_focus(self):
        self.focus_released.fire()

    def _create_axes(self):
        # limits specified as [left, bottom, width, height]
        # leave 2.5% border all around
        axes = plt.Axes(self.figure, [0.025, 0.025, 0.95, 0.95])

        # Make the axes fit the image tightly
        axes.set_xlim([0, self.image_width])
        axes.set_ylim([0, self.image_height])

        # # Don't draw tick marks and labels
        axes.set_axis_off()

        return axes

    def _do_render(self):
        self.figure = plt.figure()
        self.axes = self._create_axes()
        self.figure.add_axes(self.axes)

        self._interaction_context = InteractionContext(self)

        # Add 1 because FITS images start at pixel 1,1 while matplotlib
        # starts at 0,0
        extent = (1, self.image_width + 1, self.image_height + 1, 1)
        self.axes_image = plt.imshow(zscale(self.image_data),
                                     extent=extent,
                                     cmap=self._colormap.as_mpl_cmap())

        # Create axes for colorbar.  Make it tightly fit the image.
        divider = make_axes_locatable(self.axes)
        cax = divider.append_axes("bottom", size="5%", pad=0.05)
        self.figure.colorbar(self.axes_image, orientation="horizontal",
                             cax=cax)

    def _refresh_displayed_colormap(self):
        self.axes_image.set_cmap(self._colormap.as_mpl_cmap())
        self.axes_image.changed()
        self.display_changed.fire()


def zscale(image):
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
    z1, z2 = numdisplay.zscale.zscale(image, nsamples=1000, contrast=0.25)
    return np.clip(image, z1, z2)

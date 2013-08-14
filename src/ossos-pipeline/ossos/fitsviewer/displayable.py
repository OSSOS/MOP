__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from stsci import numdisplay

from ossos.fitsviewer.colormap import GrayscaleColorMap
from ossos.fitsviewer.exceptions import MPLViewerError
from ossos.fitsviewer.interaction import InteractionContext, Signal


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

        self.marker = None

        self.display_changed = Signal()
        self.xy_changed = Signal()
        self.focus_released = Signal()

        self._colormap = GrayscaleColorMap()

        self._mpl_event_handlers = {}
        self._interaction_context = None

    @property
    def image_data(self):
        return _image_data(self.hdulist)

    @property
    def width(self):
        return _image_width(self.hdulist)

    @property
    def height(self):
        return _image_height(self.hdulist)

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
        contrast_diff = float(-dy) / self.height
        bias_diff = float(dx) / self.width

        self._colormap.update_contrast(contrast_diff)
        self._colormap.update_bias(bias_diff)

        self._refresh_displayed_colormap()

    def reset_colormap(self):
        self._colormap.set_defaults()
        self._refresh_displayed_colormap()

    def place_marker(self, x, y, radius):
        """
        Draws a marker with the specified dimensions.  Only one marker can
        be on the image at a time, so any existing marker will be replaced.
        """
        if self.marker is not None:
            self.marker.remove_from_axes()

        self.marker = Marker(x, y, radius)
        self.marker.add_to_axes(self.axes)

        self.display_changed.fire()

    def update_marker(self, x, y, radius=None):
        if self.marker is None:
            if radius is None:
                raise MPLViewerError("No marker to update.")
            else:
                # For convenience go ahead and make one
                self.place_marker(x, y, radius)

        self.marker.center = (x, y)

        if radius is not None:
            self.marker.radius = radius

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

    def release_focus(self):
        self.focus_released.fire()

    def _apply_event_handlers(self, canvas):
        for eventname, handler in self._mpl_event_handlers.itervalues():
            canvas.mpl_connect(eventname, handler)

    def _do_render(self):
        self.figure = plt.figure()
        self.axes = self._create_axes()
        self.figure.add_axes(self.axes)

        self._interaction_context = InteractionContext(self)

        self.axes_image = plt.imshow(zscale(self.image_data),
                                     origin="lower",
                                     cmap=self._colormap.as_mpl_cmap())

        # Create axes for colorbar.  Make it tightly fit the image.
        divider = make_axes_locatable(self.axes)
        cax = divider.append_axes("bottom", size="5%", pad=0.05)
        self.figure.colorbar(self.axes_image, orientation="horizontal",
                             cax=cax)

    def _create_axes(self):
        # limits specified as [left, bottom, width, height]
        # leave 2.5% border all around
        axes = plt.Axes(self.figure, [0.025, 0.025, 0.95, 0.95])

        # Don't draw tick marks and labels
        axes.set_axis_off()

        # FITS images start at pixel 1,1 in the bottom-left corner
        axes.set_xlim([1, self.width + 1])
        axes.set_ylim([1, self.height + 1])

        return axes

    def _refresh_displayed_colormap(self):
        self.axes_image.set_cmap(self._colormap.as_mpl_cmap())
        self.axes_image.changed()
        self.display_changed.fire()


class DisplayableImageTriplet(object):
    def __init__(self, hdulist_grid):
        if len(hdulist_grid) != 3:
            raise ValueError("Grid must have 3 rows (given %d)"
                             % len(hdulist_grid))

        self.frames = map(_ImageTriplet, hdulist_grid)

        self.figure = None
        self.axes = None
        self._mpl_event_handlers = {}
        self._interaction_context = None

    def render(self, canvas=None):
        # TODO: remove duplication with singlet
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

    def _do_render(self):
        self.figure = plt.figure()
        for position, frame in enumerate(self.frames):
            frame.render(self.figure, position)


class _ImageTriplet(object):
    """
    A row of images that share an axes and colormap.  Does not have its
    own figure.
    """

    def __init__(self, hdulists):
        if len(hdulists) != 3:
            raise ValueError("Image triplet must contain 3 images (given %d)"
                             % len(hdulists))

        self.hdulists = hdulists
        self.axes = None

        self._colormap = GrayscaleColorMap()

    @property
    def width(self):
        return sum(map(_image_width, self.hdulists))

    @property
    def height(self):
        return _image_height(self.hdulists[0])

    def render(self, figure, position):
        if self.axes is None:
            self._do_render(figure, position)

    def _do_render(self, figure, position):
        self._create_axes(figure, position)

        full_image = zscale(np.concatenate(map(_image_data, self.hdulists),
                                           axis=1))

        # TODO: remove duplication with singlet
        # Add 1 because FITS images start at pixel 1,1 while matplotlib
        # starts at 0,0
        extent = (1, self.width + 1, self.height + 1, 1)
        self.axes_image = self.axes.imshow(full_image,
                                           extent=extent,
                                           cmap=self._colormap.as_mpl_cmap())

    def _create_axes(self, figure, position):
        # TODO: remove duplication with singlet
        # limits specified as [left, bottom, width, height]
        # leave 2.5% border all around
        border = 0.025
        width = 1 - 2 * border
        height = (1 - 2 * border) / 3
        bottom = border + position * height
        self.axes = figure.add_axes([border, bottom, width, height])

        # Make the axes fit the image tightly
        self.axes.set_xlim([0, self.width])
        self.axes.set_ylim([0, self.height])

        # Don't draw tick marks and labels
        self.axes.set_axis_off()


class Marker(object):
    def __init__(self, x, y, radius):
        self.circle = plt.Circle((x, y), radius, color="b", fill=False)

        linewidth = 0.5
        self.vline = plt.Line2D((x, x), (y - radius, y + radius),
                                linewidth=linewidth)
        self.hline = plt.Line2D((x - radius, x + radius), (y, y),
                                linewidth=linewidth)

    @property
    def center(self):
        return self.circle.center

    @center.setter
    def center(self, new_center):
        self.circle.center = new_center
        self._update_cross()

    @property
    def radius(self):
        return self.circle.radius

    @radius.setter
    def radius(self, new_radius):
        self.circle.radius = new_radius
        self._update_cross()

    def add_to_axes(self, axes):
        axes.add_patch(self.circle)
        self.vline.set_transform(axes.transData)
        self.hline.set_transform(axes.transData)
        axes.lines.extend([self.vline, self.hline])

    def remove_from_axes(self):
        self.circle.remove()

    def contains(self, event):
        return self.circle.contains(event)

    def _update_cross(self):
        x, y = self.center
        self.vline.set_data((x, x), (y - self.radius, y + self.radius))
        self.hline.set_data((x - self.radius, x + self.radius), (y, y))


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


def _image_width(hdulist):
    return _image_shape(hdulist)[1]


def _image_height(hdulist):
    return _image_shape(hdulist)[0]


def _image_shape(hdulist):
    return _image_data(hdulist).shape


def _image_data(hdulist):
    return hdulist[0].data

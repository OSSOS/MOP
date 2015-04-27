from cStringIO import StringIO
import logging
from astropy import units
from astropy.io import fits
import ds9

__author__ = "David Rusk <drusk@uvic.ca>"

import matplotlib.pyplot as plt

from .colormap import GrayscaleColorMap
from .exceptions import MPLViewerError
from .interaction import InteractionContext, Signal


class Displayable(object):
    """
    An image or group of images which can be displayed.

    Attributes:
      figure: matplotlib figure the images are placed on.
    """

    def __init__(self):
        self.figure = None
        # self.figure = plt.figure()  [stop using the matplotlib plt]
        self.canvas = None
        self.rendered = False

    @property
    def width(self):
        raise NotImplementedError()

    @property
    def height(self):
        raise NotImplementedError()

    def render(self, canvas=None):
        if not self.rendered:
            self._do_render()

        # if canvas is None:
        #     pass
        #     #plt.show()
        # else:
        #     self.canvas = canvas
        #     canvas.figure = self.figure
        #
        #     parent_size = canvas.GetClientSize()
        #
        #     figure_dpi = self.figure.get_dpi()
        #     self.figure.set_size_inches(parent_size[0] / figure_dpi,
        #                                 parent_size[1] / figure_dpi)
        #
        #     self._apply_event_handlers(canvas)

    def redraw(self):
        pass
        # if self.canvas is not None:
        #     self.canvas.draw()

    def place_error_ellipse(self, x, y, a, b, pa, color='y'):
        pass

    def reset_colormap(self):
        pass

    def toggle_reticule(self):
        pass

    def _do_render(self):
        raise NotImplementedError()

    def _apply_event_handlers(self, canvas):
        pass


class ImageSinglet(object):
    """
    A single image on a matplotlib axes.  Provides interaction and is markable.

    """

    def __init__(self, hdulist, figure, rect):
        self.hdulist = hdulist
        self.figure = figure
        self.axes = self._create_axes(rect)
        #self.figure.add_axes(self.axes)

        self.marker = None

        self.display_changed = Signal()
        self.xy_changed = Signal()
        self.focus_released = Signal()

        self._colormap = GrayscaleColorMap()
        self._mpl_event_handlers = {}
        self._interaction_context = None
        self.number_of_images_displayed = 0
        self.frame_number = None

    @property
    def width(self):
        return _image_width(self.hdulist)

    @property
    def height(self):
        return _image_height(self.hdulist)

    def show_image(self, colorbar=False):
        # start xpans if needed
        ds9.ds9_xpans()
        # start ds9 if need, or connect to existing
        display = None
        cnt = 0
        while display is None and cnt < 10:
            cnt += 1
            try:
                display = ds9.ds9(target='validate')
                cnt = 10
            except ValueError as ve:
                logging.error(str(ve))
                pass
        if self.frame_number is None:
            # display.set('frame delete all')
            display.set('frame new')
            display.set('scale zscale')
            display.set('cmap invert yes')
            f = StringIO()
            self.hdulist.writeto(f, output_verify='ignore')
            f.flush()
            f.seek(0)
            hdulist = fits.open(f)
            for hdu in hdulist:
                del(hdu.header['PV*'])
            try:
                display.set_pyfits(hdulist)
            except ValueError as ex:
                logging.error("Failed while trying to display: {}".format(hdulist))
                logging.error("{}".format(ex))
            f.close()
            del(hdulist)
            self.frame_number = display.get('frame frameno')
            display.set('frame center {}'.format(self.frame_number))
            display.set('zoom to fit')
            display.set('wcs align yes')
        display.set('frame frameno {}'.format(self.frame_number))

        self._interaction_context = InteractionContext(self)

        self.number_of_images_displayed += 1

    def clear_markers(self):
        display = ds9.ds9(target='valdiate')
        display.set('regions delete all')

    def place_marker(self, x, y, radius, colour="b"):
        """
        Draws a marker with the specified dimensions.  Only one marker can
        be on the image at a time, so any existing marker will be replaced.
        """
        display = ds9.ds9(target='validate')
        colour_string = {'r': 'red', 'b': 'blue'}.get(colour, 'green')
        display.set('regions', 'image; circle({},{},{}) # color={}'.format(x,y,radius,colour_string))

        #if self.marker is not None:
        #    self.marker.remove_from_axes(self.axes)
        #
        #self.marker = Marker(x, y, radius, colour=colour)
        #self.marker.add_to_axes(self.axes)

        self.display_changed.fire()

    def place_error_ellipse(self, x, y, a, b, pa, color='b'):
        """
        Draws an ErrorEllipse with the given dimensions.  Can not be moved later.
        """
        display = ds9.ds9(target='validate')
        # display.set('regions delete all')
        ell = 'ellipse({},{},{},{},{}'.format(x, y, a, b, pa.to(units.degree).value + 90)
        display.set('regions', 'image ;{}'.format(ell))
        #self.error_ellipse = ErrEllipse(x, y, a, b, pa, color=color)
        #self.error_ellipse.add_to_axes(self.axes)
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

    def release_focus(self):
        self.focus_released.fire()

    def update_colormap(self, dx, dy):
        contrast_diff = float(-dy) / self.height
        bias_diff = float(dx) / self.width

        self._colormap.update_contrast(contrast_diff)
        self._colormap.update_bias(bias_diff)

        self._refresh_displayed_colormap()

    def reset_colormap(self):
        self._colormap.set_defaults()
        self._refresh_displayed_colormap()

    def toggle_reticule(self):
        self.marker.toggle_reticule()
        self.display_changed.fire()

    def is_event_in_axes(self, event):
        return self.axes == event.inaxes

    def register_mpl_event_handler(self, eventname, handler):
        return 0
#        handler_id = self.figure.canvas.mpl_connect(eventname, handler)
#        self._mpl_event_handlers[handler_id] = (eventname, handler)
#        return handler_id

    def deregister_mpl_event_handler(self, id_):
        return
#        self.figure.canvas.mpl_disconnect(id_)
#        del self._mpl_event_handlers[id_]

    def apply_event_handlers(self, canvas):
        for eventname, handler in self._mpl_event_handlers.itervalues():
            canvas.mpl_connect(eventname, handler)

    def _create_axes(self, rect):
        """
        Args:
          rect: [left, bottom, width, height]
            Used to construct the matplotlib axes.
        """
        return None

    def _refresh_displayed_colormap(self):
        self.axes_image.set_cmap(self._colormap.as_mpl_cmap())
        self.axes_image.changed()
        self.display_changed.fire()


class DisplayableImageSinglet(Displayable):
    """
    A single displayable image.

    Attributes:
        hdulist: the FITS image being displayed.

        See also Displayable's attributes.
    """

    def __init__(self, hdulist):
        """
        Args:
          hdulist: astropy.io.fits.HDUList
            The FITS image to be displayed.
        """
        super(DisplayableImageSinglet, self).__init__()

        self.hdulist = hdulist
        self.image_singlet = ImageSinglet(self.hdulist, self.figure,
                                          [0.025, 0.025, 0.95, 0.95])
        self.image_singlet.display_changed.connect(self.redraw)
        self.marker_placed = False
        self.ellipse_placed = False
        self.annulus_placed = False

    @property
    def xy_changed(self):
        return self.image_singlet.xy_changed

    @property
    def focus_released(self):
        return self.image_singlet.focus_released

    def place_marker(self, x, y, radius, colour="b"):
        if not self.marker_placed:
            self.image_singlet.place_marker(x, y, radius, colour=colour)
            self.marker_placed = True

    def place_annulus(self, x, y, radii, colour='b'):
        if not self.annulus_placed:
            for radius in radii:
                self.image_singlet.place_marker(x, y, radius, colour=colour)
            self.annulus_placed = True

    def place_error_ellipse(self, x, y, a, b, pa, color='b'):
        if not self.ellipse_placed:
            self.image_singlet.place_error_ellipse(x, y, a, b, pa, color=color)
            self.ellipse_placed = True

    def reset_colormap(self):
        self.image_singlet.reset_colormap()

    def toggle_reticule(self):
        self.image_singlet.toggle_reticule()

    def _do_render(self):
        self.image_singlet.show_image(colorbar=True)

    def _apply_event_handlers(self, canvas):
        self.image_singlet.apply_event_handlers(canvas)


class DisplayableImageTriplet(Displayable):
    def __init__(self, cutout_grid):
        super(DisplayableImageTriplet, self).__init__()

        if cutout_grid.shape != (3, 3):
            raise ValueError("Must be a 3 by 3 grid (was given %d by %d)"
                             % (cutout_grid.shape[0], cutout_grid.shape[1]))

        self.cutout_grid = cutout_grid

        d  = ds9.ds9('validate')
        d.set('frame delete all')
        d.set('tile yes')
        d.set('tile grid layout 3 3')
        self.frames = []
        num_frames, num_times = cutout_grid.shape
        for frame_index in range(num_frames):
            frame = []
            for time_index in range(num_times):
                singlet = ImageSinglet(cutout_grid.get_hdulist(frame_index, time_index),
                                       self.figure,
                                       get_rect(cutout_grid.shape, frame_index, time_index,
                                                spacing=0))
                singlet.display_changed.connect(self.redraw)
                frame.append(singlet)

            self.frames.append(frame)

    def get_singlet(self, frame_index, time_index):
        return self.frames[frame_index][time_index]

    def iter_singlets(self):
        for frame in self.frames:
            for singlet in frame:
                yield singlet

    def reset_colormap(self):
        for singlet in self.iter_singlets():
            singlet.reset_colormap()

    def toggle_reticule(self):
        for singlet in self.iter_singlets():
            singlet.toggle_reticule()

    def _do_render(self):
        for singlet in self.iter_singlets():
            singlet.show_image(colorbar=False)

    def _apply_event_handlers(self, canvas):
        for singlet in self.iter_singlets():
            singlet.apply_event_handlers(canvas)


def get_rect(shape, frame_index, time_index, border=0.025, spacing=0.01):
    rows, cols = shape

    width = (1.0 - 2 * border - (cols - 1) * spacing) / cols
    height = (1.0 - 2 * border - (rows - 1) * spacing) / rows

    left = border + (width + spacing) * time_index
    bottom = border + (height + spacing) * (rows - frame_index - 1)

    return [left, bottom, width, height]


class ErrEllipse(object):
    """
    A class for creating and drawing an ellipse in matplotlib.
    """
    def __init__(self, x_cen, y_cen, a, b, pa, color='b'):
        """
        :param x_cen: x coordinate at center of the ellipse
        :param y_cen: y coordinate at center of the ellipse
        :param a: size of semi-major axes of the ellipse
        :param b: size of semi-minor axes of the ellipse
        :param pa: position angle of a to x  (90 ==> a is same orientation as x)
        """

        self.center = (x_cen, y_cen)
        self.a = max(a, 10)
        self.b = max(b, 10)
        self.pa = pa
        self.angle = self.pa - 90


    def add_to_axes(self, axes):
        return None


class Depricated_Marker(object):
    def __init__(self, x, y, radius, colour="b"):
        self.circle = plt.Circle((x, y), radius, color=colour, fill=False)

        self.crosshair_scaling = 2

        crosshair_colour = "w"
        linewidth = 1

        self.left_hair = plt.Line2D(
            self._get_left_x_extent(),
            self._get_horizontal_y_extent(),
            color=crosshair_colour,
            linewidth=linewidth)

        self.right_hair = plt.Line2D(
            self._get_right_x_extent(),
            self._get_horizontal_y_extent(),
            color=crosshair_colour,
            linewidth=linewidth)

        self.top_hair = plt.Line2D(
            self._get_vertical_x_extent(),
            self._get_top_y_extent(),
            color=crosshair_colour,
            linewidth=linewidth)

        self.bottom_hair = plt.Line2D(
            self._get_vertical_x_extent(),
            self._get_bottom_y_extent(),
            color=crosshair_colour,
            linewidth=linewidth)

    @property
    def x(self):
        return self.circle.center[0]

    @property
    def y(self):
        return self.circle.center[1]

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

    @property
    def lines(self):
        return [self.left_hair, self.right_hair,
                self.top_hair, self.bottom_hair]

    def add_to_axes(self, axes):
        def transform(line):
            line.set_transform(axes.transData)

        axes.add_patch(self.circle)

        for line in self.lines:
            transform(line)

        axes.lines.extend(self.lines)

    def remove_from_axes(self, axes):
        self.circle.remove()

        for line in self.lines:
            axes.lines.remove(line)

    def contains(self, event):
        return self.circle.contains(event)

    def toggle_reticule(self):
        self.circle.set_visible(not self.circle.get_visible())
        for line in self.lines:
            line.set_visible(not line.get_visible())

    def _get_vertical_x_extent(self):
        return self.x, self.x

    def _get_bottom_y_extent(self):
        bottom = self.y - self.radius
        top = bottom + self.radius / self.crosshair_scaling
        return bottom, top

    def _get_top_y_extent(self):
        top = self.y + self.radius
        bottom = top - self.radius / self.crosshair_scaling
        return bottom, top

    def _get_horizontal_y_extent(self):
        return self.y, self.y

    def _get_left_x_extent(self):
        left = self.x - self.radius
        right = left + self.radius / self.crosshair_scaling
        return left, right

    def _get_right_x_extent(self):
        right = self.x + self.radius
        left = right - self.radius / self.crosshair_scaling
        return left, right

    def _update_cross(self):
        self.left_hair.set_data(
            self._get_left_x_extent(),
            self._get_horizontal_y_extent())

        self.right_hair.set_data(
            self._get_right_x_extent(),
            self._get_horizontal_y_extent())

        self.top_hair.set_data(
            self._get_vertical_x_extent(),
            self._get_top_y_extent())

        self.bottom_hair.set_data(
            self._get_vertical_x_extent(),
            self._get_bottom_y_extent())


def _image_width(hdulist):
    return _image_shape(hdulist)[1]

def _image_height(hdulist):
    return _image_shape(hdulist)[0]

def _image_shape(hdulist):
    return _image_data(hdulist).shape

def _image_data(hdulist):
    return hdulist[0].data

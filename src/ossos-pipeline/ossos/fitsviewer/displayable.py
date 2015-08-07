import copy
import logging
import tempfile
from astropy import units
from astropy.coordinates import SkyCoord
from astropy.units import Quantity
from .colormap import GrayscaleColorMap
from .exceptions import MPLViewerError
from .interaction import Signal


class Displayable(object):
    """
    An image or group of images which can be displayed.

    Attributes:
      figure: matplotlib figure the images are placed on.
    """

    def __init__(self, display):
        self.figure = None
        # self.figure = plt.figure()  [stop using the matplotlib plt]
        self.canvas = None
        self.display = display
        self.rendered = False
        self.aligned = False

    @property
    def width(self):
        raise NotImplementedError()

    @property
    def height(self):
        raise NotImplementedError()

    def align(self, pos):
        if not self.aligned:
            self._do_align(pos)

    def render(self):
        if not self.rendered:
            self._do_render()

    def redraw(self):
        pass

    def place_error_ellipse(self, sky_coord, uncertainty_ellipse, color='g'):
        pass

    def reset_colormap(self):
        pass

    def toggle_reticule(self):
        pass

    def _do_render(self):
        raise NotImplementedError()

    def _do_align(self, pos):
        raise NotImplementedError()

    def _apply_event_handlers(self, canvas):
        pass


class ImageSinglet(object):
    """
    A single image on a matplotlib axes.  Provides interaction and is markable.

    """

    def __init__(self, hdulist):
        self.hdulist = hdulist

        self.marker = None
        self._display = None

        self.display_changed = Signal()
        self.xy_changed = Signal()
        self.focus_released = Signal()
        self.pos = None
        self._colormap = GrayscaleColorMap()
        self._mpl_event_handlers = {}
        self.frame_number = None

    @property
    def width(self):
        return _image_width(self.hdulist)

    @property
    def height(self):
        return _image_height(self.hdulist)

    def pan(self, ds9, pos):
        self.pos = pos
        x = pos.ra.to(units.degree).value
        y = pos.dec.to(units.degree).value
        ds9.set("pan to {} {} wcs fk5".format(x, y))

    def show_image(self, ds9, colorbar=False):
        display = ds9

        if self.frame_number is None:
            display.set('frame new')

            # create a copy of the image that does not have Gwyn's PV keywords, ds9 fails on those.
            hdulist = copy.copy(self.hdulist)
            for hdu in hdulist:
                del (hdu.header['PV*'])
            # place in a temporary file for ds9 to use, this must be an on disk file
            f = tempfile.NamedTemporaryFile(suffix=".fits")
            hdulist.writeto(f, output_verify='ignore')
            f.flush()
            f.seek(0)

            # load image into the display
            try:
                display.set('mosaicimage {}'.format(f.name))
                while display.get('frame has fits') != 'yes':
                    print "Waiting for image to load."
                    pass
                display.set('zoom to fit')
            except ValueError as ex:
                logging.error("Failed while trying to display: {}".format(hdulist))
                logging.error("{}".format(ex))

            # clear up the loose bits.
            f.close()
            del f
            del hdulist
            self.frame_number = display.get('frame')
            display.reset_preferences()
        else:
            display.set('frame frameno {}'.format(self.frame_number))
            if self.pos is not None:
                try:
                    pos = ds9.get("pan wcs degrees").split()
                    pos = SkyCoord(pos[0], pos[1], unit=units.degree)
                    if pos.separation(self.pos) > 60*units.arcsec:
                        self.pan(ds9, self.pos)
                except Exception as ex:
                    print "{}".format(ex)



    @staticmethod
    def clear_markers(ds9):
        display = ds9
        display.set('regions delete all')

    def place_marker(self, x, y, radius, colour="b", ds9=None):
        """
        Draws a marker with the specified dimensions.  Only one marker can
        be on the image at a time, so any existing marker will be replaced.
        """
        display = ds9
        colour_string = {'r': 'red', 'b': 'blue'}.get(colour, 'green')
        if isinstance(x, Quantity) and x.unit == units.degree:
            display.set('regions', 'wcs; circle({},{},{}) # color={}'.format(x.value, y.value,
                                                                             radius, colour_string))
        else:
            display.set('regions', 'image; circle({},{},{}) # color={}'.format(x, y, radius, colour_string))

        self.display_changed.fire()

    def place_error_ellipse(self, sky_coord, uncertainty_ellipse, colour='b', ds9=None):
        """
        Draws an ErrorEllipse with the given dimensions.  Can not be moved later.
        """
        display = ds9
        ellipse = 'ellipse({},{},{},{},{})'.format(sky_coord.ra.to(units.degree).value,
                                              sky_coord.dec.to(units.degree).value,
                                              max(uncertainty_ellipse.a.to(units.degree).value, 0.0005),
                                              max(uncertainty_ellipse.b.to(units.degree).value, 0.0005),
                                              uncertainty_ellipse.pa.to(units.degree).value + 90)
        colour_string = {'r': 'red', 'b': 'blue', 'y': 'yellow'}.get(colour, 'green')
        display.set('regions','fk5; {} # color={}'.format(ellipse, colour_string))
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
        raise NotImplemented()

    def register_mpl_event_handler(self, eventname, handler):
        return 0

    def deregister_mpl_event_handler(self, id_):
        pass

    def apply_event_handlers(self, canvas):
        for eventname, handler in self._mpl_event_handlers.itervalues():
            canvas.mpl_connect(eventname, handler)

    def _refresh_displayed_colormap(self):
        raise NotImplemented()


class DisplayableImageSinglet(Displayable):
    """
    A single displayable image.

    Attributes:
        hdulist: the FITS image being displayed.

        See also Displayable's attributes.
    """

    def __init__(self, hdulist, display=None):
        """
        Args:
          hdulist: astropy.io.fits.HDUList
            The FITS image to be displayed.
        """
        super(DisplayableImageSinglet, self).__init__(display)
        self.image_singlet = ImageSinglet(hdulist)
        self.image_singlet.display_changed.connect(self.redraw)
        self.marker_placed = False
        self.ellipse_placed = False
        self.annulus_placed = False
        self.pos = None

    @property
    def xy_changed(self):
        return self.image_singlet.xy_changed

    @property
    def focus_released(self):
        return self.image_singlet.focus_released

    def place_marker(self, x, y, radius, colour="g"):
        if not self.marker_placed:
            self.image_singlet.place_marker(x, y, radius, colour=colour, ds9=self.display)
            self.marker_placed = True

    def place_annulus(self, x, y, radii, colour='g'):
        if not self.annulus_placed:
            for radius in radii:
                self.image_singlet.place_marker(x, y, radius, colour=colour, ds9=self.display)
            self.annulus_placed = True

    def place_error_ellipse(self, sky_coord, uncertainty_ellipse, colour='y'):
        if not self.ellipse_placed:
            self.image_singlet.place_error_ellipse(sky_coord, uncertainty_ellipse, colour=colour, ds9=self.display)
            self.ellipse_placed = True

    def reset_colormap(self):
        self.image_singlet.reset_colormap()

    def toggle_reticule(self):
        self.image_singlet.toggle_reticule()

    def _do_render(self):
        self.image_singlet.show_image(ds9=self.display, colorbar=False)

    def _do_align(self, pos):
        self.pos = pos
        if not self.aligned:
            self.image_singlet.pan(ds9=self.display, pos=pos)
        self.aligned = True

    def _apply_event_handlers(self, canvas):
        self.image_singlet.apply_event_handlers(canvas)


class DisplayableImageTriplet(Displayable):

    def width(self):
        pass

    def height(self):
        pass

    def __init__(self, cutout_grid, display):
        super(DisplayableImageTriplet, self).__init__(display)

        if cutout_grid.shape != (3, 3):
            raise ValueError("Must be a 3 by 3 grid (was given %d by %d)"
                             % (cutout_grid.shape[0], cutout_grid.shape[1]))

        self.cutout_grid = cutout_grid
        d = self.display
        d.set('frame delete all')
        d.set('tile yes')
        d.set('tile grid layout 3 3')
        self.frames = []
        num_frames, num_times = cutout_grid.shape
        for frame_index in range(num_frames):
            frame = []
            for time_index in range(num_times):
                singlet = ImageSinglet(cutout_grid.get_hdulist(frame_index, time_index))
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


def _image_width(hdulist):
    return _image_shape(hdulist)[1]


def _image_height(hdulist):
    return _image_shape(hdulist)[0]


def _image_shape(hdulist):
    return _image_data(hdulist).shape


def _image_data(hdulist):
    return hdulist[0].data

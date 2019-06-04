import logging
import tempfile
import inspect

from astropy import units
from astropy.coordinates import SkyCoord
from astropy.io import fits
from astropy.units import Quantity

from .colormap import GrayscaleColorMap
from .interaction import Signal
from ..astrom import Ellipse


class Region(object):
    """
    A DS9 region object.

    given a 'point' creates a circle region at that point.

    style can be one of 'cirlce', 'ellipse', 'annulus', 'point'

    if 'circle' then the radius of the circle is passed as the shape.
    if 'ellipse' then an ellipse object should be passed as the 'shape'
    if 'annulus' then a set of annulus sizes should be passed as the 'shape'
    if 'point' then the radius is ignored. point(1:37:02.476,+12:35:42.12) # point=x

    """

    def __init__(self, point, style='circle', colour='g', shape=10, option=""):
        """
        :param shape: The parameters that define the shape.
        :type shape: int, list
        """
        self._point = point
        self._coo = None
        self._colour = None
        self.colour = colour
        self.style = style
        self.shape = shape
        self.option = option

    def __str__(self):
        s = ""
        for c in self:
            s += str(c)+" "
        return s

    def __iter__(self):
        if self.style == 'point':
            return iter(('regions', '{}; {}({},{}) # color={} point=x '.format(self.coosys,
                                                                               self.style,
                                                                               self.point[0],
                                                                               self.point[1],
                                                                               self.colour)))
        try:
            if isinstance(self.shape, Ellipse):
                shape = self.shape
            elif isinstance(self.shape[0], Quantity):
                shape = ",".join(['{}"'.format(p.to(units.arcsec).value) for p in self.shape])
            else:
                shape = ",".join(['{}'.format(p) for p in self.shape])
        except Exception:
            try:
                shape = '{}"'.format(self.shape.to(units.arcsec).value)
            except:
                shape = '{}"'.format(0.185*self.shape)

        return iter(('regions', '{}; {}({},{},{}) # color={} {}'.format(self.coosys,
                                                                        self.style,
                                                                        self.point[0],
                                                                        self.point[1],
                                                                        shape,
                                                                        self.colour,
                                                                        self.option)))

    @property
    def point(self):
        if not self._coo:
            if isinstance(self._point, SkyCoord):
                self._coo = self._point.ra.degree, self._point.dec.degree
            elif isinstance(self._point[0], Quantity):
                try:
                    self._coo = self._point[0].to(units.degree).value, self._point[1].to(units.degree).value
                except:
                    self._coo = self._point[0].value, self._point[1].value
            else:
                self._coo = self._point
        return self._coo

    @property
    def coosys(self):
        if isinstance(self._point, SkyCoord):
            return "wcs"
        if isinstance(self._point[0], Quantity):
            try:
                self._point[0].to(units.degree)
                return "wcs"
            except:
                return "image"
        return "image"

    @property
    def colour(self):
        """
        The colour of the marker to create.
        """
        return self._colour

    @colour.setter
    def colour(self, colour):
        self._colour = {'r': 'red', 'b': 'blue', 'y': 'yellow', 'c': 'cyan', 'g': 'green'}.get(colour, colour)


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
        self.focus = None
        self.mark_reticule = True
        self._annulus_placed = False
        self._ellipse_placed = False
        self._marker_placed = False

    @property
    def width(self):
        raise NotImplementedError()

    @property
    def height(self):
        raise NotImplementedError()

    def pan_to(self, pos):
        raise NotImplementedError()

    def render(self):
        if not self.rendered:
            self._do_render()

    def redraw(self):
        pass

    def place_ellipse(self, sky_coord, ellipse, colour='g', dash=0):
        if not self.display or self._ellipse_placed or not self.mark_reticule:
            return
        r = Region(sky_coord, style='ellipse', colour=colour, shape=ellipse, option='dash={} width=2'.format(dash))
        self.display.set(*r)
        self._ellipse_placed = True

    def place_annulus(self, x, y, annuli, colour='b'):

        import inspect
        curframe = inspect.currentframe()

        if not self.display or self._annulus_placed or not self.mark_reticule:
            return
        try:
            r = Region((x, y), style='annulus', colour=colour, shape=annuli)
            self.display.set(*r)
            r = Region((x, y), style='point', colour=colour)
            self.display.set(*r)
        except Exception as ex:
            print(ex)

        self._annulus_placed = True

    def place_marker(self, x, y, radius=10, colour='b', force=False):

        import inspect
        curframe = inspect.currentframe()
        calframe = inspect.getouterframes(curframe, 2)
        logging.debug("{} called place_maker".format(calframe[1][3]))

        if not force and (not self.display or self._marker_placed or not self.mark_reticule):
            return
        self.display.set(*Region((x, y), style='circle', colour=colour, shape=radius))
        self._marker_placed = True

    def clear_markers(self):
        self.display.set('regions delete all')
        self._marker_placed = self._annulus_placed = self._ellipse_placed = False

    def reset_colormap(self):
        pass

    def toggle_reticule(self):
        self.clear_markers()
        self.mark_reticule = not self.mark_reticule
        return self.mark_reticule

    def _do_render(self):
        raise NotImplementedError()

    def _do_move_focus(self):
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

    def show_image(self, ds9):
        display = ds9

        if self.frame_number is None:
            display.new_frame()

            # create a copy of the image that does not have Gwyn's PV keywords, ds9 fails on those.
            f = tempfile.NamedTemporaryFile(suffix=".fits")
            self.hdulist.writeto(f, output_verify='ignore')
            f.flush()
            hdulist = fits.open(f.name, mode='update')
            for hdu in hdulist:
                del (hdu.header['PV*'])
            # place in a temporary file for ds9 to use, this must be an on disk file
            hdulist.close()
            f.seek(0)

            # load image into the display
            try:
                display.set('mosaicimage {}'.format(f.name))
                while display.get('frame has fits') != 'yes':
                    print("Waiting for image to load.")
                    pass
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

    def update_marker(self, x, y, radius=None):
        raise NotImplementedError('update_marker')

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

    def is_event_in_axes(self, event):
        raise NotImplemented()

    def register_mpl_event_handler(self, eventname, handler):
        return 0

    def deregister_mpl_event_handler(self, id_):
        pass

    def apply_event_handlers(self, canvas):
        for eventname, handler in self._mpl_event_handlers.values():
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
        self._focus = None
        self.minimum_pan = 0 * units.arcsec

    @property
    def xy_changed(self):
        return self.image_singlet.xy_changed

    @property
    def focus_released(self):
        return self.image_singlet.focus_released

    def reset_colormap(self):
        self.image_singlet.reset_colormap()

    def _do_render(self):
        self.image_singlet.show_image(ds9=self.display)
        # self._do_move_focus()

    @property
    def aligned(self):
        if self.focus is None:
            return False
        focus = self.display.get("pan wcs degrees").split()
        focus = SkyCoord(focus[0], focus[1], unit=units.degree)
        return focus.separation(self.focus) < self.minimum_pan

    @property
    def focus(self):
        return self._focus

    @focus.setter
    def focus(self, focus):
        if not focus:
            return
        try:
            if isinstance(focus, SkyCoord):
                self._focus = focus
            else:
                self._focus = SkyCoord(focus[0], focus[1])
        except Exception as ex:
            print("Focus setting failed to convert focus tuple {} to SkyCoord: {}".format(focus, ex))
            self._focus = focus

    def _do_move_focus(self):
        if self.focus is None:
            return
        if not self.aligned:
            #print "Panning the image to focus position"
            self.display.set("pan to {} {} wcs fk5".format(self.focus.ra.degree,
                                                           self.focus.dec.degree))

    def pan_to(self, pos):
        self.focus = pos
        self._do_move_focus()

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



def _image_width(hdulist):
    return _image_shape(hdulist)[1]


def _image_height(hdulist):
    return _image_shape(hdulist)[0]


def _image_shape(hdulist):
    return _image_data(hdulist).shape


def _image_data(hdulist):
    return hdulist[0].data

import copy
import logging

from astropy import units
from astropy.coordinates import SkyCoord

from ossos.astrom import SourceReading
from ossos.downloads.cutouts.focus import SingletFocusCalculator
from ossos.downloads.cutouts.source import SourceCutout
from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"


class WxMPLFitsViewer(object):
    """
    Display FITS images using ds9.
    """

    def __init__(self, parent, display):
        self.parent = parent

        self.current_cutout = None
        self.current_displayable = None
        self._ds9 = display
        self._displayables_by_cutout = {}
        self.mark_source = True
        self.mark_prediction = False

    @property
    def ds9(self):
        return self._ds9

    def display(self, cutout):
        """
        :param cutout: source cutout object to be display
        :type cutout: source.SourceCutout
        """
        logging.debug("Current display list contains: {}".format(self._displayables_by_cutout.keys()))
        logging.debug("Looking for {}".format(cutout))
        assert isinstance(cutout, SourceCutout)
        if cutout in self._displayables_by_cutout:
            displayable = self._displayables_by_cutout[cutout]
        else:
            displayable = self._create_displayable(cutout)
            self._displayables_by_cutout[cutout] = displayable

        self._detach_handlers(self.current_displayable)

        self.current_cutout = cutout
        self.current_displayable = displayable

        self._attach_handlers(self.current_displayable)

        self._do_render(self.current_displayable)

        self.mark_apertures(cutout, pixel=False)

        self.draw_uncertainty_ellipse(cutout)

    def clear(self):
        self.ds9.set("frame delete all")

    def draw_uncertainty_ellipse(self, cutout):
        """
        Draws an ErrEllipse with the spcified dimensions.  Only one ErrEllipse can be drawn and
        only once (not movable).
        """
        if not self.mark_prediction:
            return
        colour = cutout.reading.from_input_file and 'b' or 'g'
        colour = cutout.reading.null_observation and 'c' or colour
        dash = cutout.reading.null_observation and 1 or 0
        sky_coord = cutout.reading.sky_coord
        x, y, extno = cutout.world2pix(sky_coord.ra.to(units.degree).value,
                                       sky_coord.dec.to(units.degree).value)
        hdulist = copy.copy(cutout.hdulist)
        for hdu in hdulist[1:]:
            del hdu.header['PV*']
        ra, dec = hdulist[extno].wcs.xy2sky(x, y)
        uncertainty_ellipse = cutout.reading.uncertainty_ellipse
        self.current_displayable.place_ellipse((ra, dec), uncertainty_ellipse, colour=colour, dash=dash)

    def refresh_markers(self):
        self.mark_apertures(self.current_cutout)

    def mark_apertures(self, cutout, pixel=False):
        pass

    def release_focus(self):
        self.parent.SetFocus()

    def reset_colormap(self):
        if self.current_displayable is not None:
            self.current_displayable.reset_colormap()

    def toggle_reticule(self):
        result = self.current_displayable.toggle_reticule()
        if result:
            self.mark_apertures(self.current_cutout, pixel=False)
            self.draw_uncertainty_ellipse(self.current_cutout)

    def _attach_handlers(self, displayable):
        pass

    def _detach_handlers(self, displayable):
        pass

    def _create_displayable(self, cutout):
        raise NotImplementedError()

    def align(self, cutout, reading, source):
        """
        Set the display center to the reference point.

        @param cutout:  The cutout to align on
        @type cutout: SourceCutout
        @param reading:  The reading this cutout is from
        @type reading: SourceReading
        @param source: The source the reading is from
        @type source: Source
        @return:
        """
        if not self.current_displayable:
            return
        if not self.current_displayable.aligned:
            focus_calculator = SingletFocusCalculator(source)
            logger.debug("Got focus calculator {} for source {}".format(focus_calculator, source))
            focus = cutout.flip_flip(focus_calculator.calculate_focus(reading))
            focus = cutout.get_pixel_coordinates(focus)
            focus = cutout.pix2world(focus[0], focus[1])
            focus_sky_coord = SkyCoord(focus[0], focus[1])
            self.current_displayable.pan_to(focus_sky_coord)

    @staticmethod
    def _do_render(displayable):
        displayable.render()

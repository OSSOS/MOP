from ossos.downloads.cutouts.focus import SingletFocusCalculator
from ossos.downloads.cutouts.source import SourceCutout
from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"
import logging

from ..downloads.cutouts import source


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

    @property
    def ds9(self):
        return self._ds9

    def display(self, cutout, mark_source=True, pixel=False):
        """

        :param cutout: source cutout object to be display
        :type cutout: source.SourceCutout
        :param mark_source: should the sources associated with the cutout be display?
        :type mark_source: bool
        :param pixel: use the source coordinates in pixels (True) or RA/DEC (False)
        :type pixel: bool
        """
        logging.debug("Current display list contains: {}".format(self._displayables_by_cutout.keys()))
        logging.debug("Looking for {}".format(cutout))

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

        if mark_source:
            self.mark_apertures(cutout, pixel=pixel)

    def clear(self):
        self.ds9.set("frame delete all")

    def draw_error_ellipse(self, x, y, a, b, pa, color='y'):
        """
        Draws an ErrEllipse with the spcified dimensions.  Only one ErrEllipse can be drawn and
        only once (not movable).
        """
        self.current_displayable.place_error_ellipse(x, y, a, b, pa, color=color)

    def mark_sources(self, cutout):
        pass

    def refresh_markers(self):
        self.mark_sources(self.current_cutout)

    def mark_apertures(self, cutout, pixel=False):
        pass

    def release_focus(self):
        self.parent.SetFocus()

    def reset_colormap(self):
        if self.current_displayable is not None:
            self.current_displayable.reset_colormap()

    def toggle_reticule(self):
        self.current_displayable.toggle_reticule()

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
        if not self.current_displayable.pos:
            focus_calculator = SingletFocusCalculator(source)
            logger.debug("Got focus calculator {} for source {}".format(focus_calculator, source))
            focus = focus_calculator.calculate_focus(reading)
            assert isinstance(cutout, SourceCutout)
            focus = cutout.get_pixel_coordinates(focus, cutout.extno)
            focus_sky_coord = cutout.hdulist[cutout.extno].wcs.xy2sky(focus[0], focus[1])
            self.current_displayable.align(focus_sky_coord)

    def _do_render(self, displayable):
        displayable.render()

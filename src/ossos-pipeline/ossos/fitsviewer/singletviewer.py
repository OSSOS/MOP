import ds9

__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.displayable import DisplayableImageSinglet
from ossos.fitsviewer.interaction import Signal
from ossos.gui import logger


class SingletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent):
        super(SingletViewer, self).__init__(parent)
        self.xy_changed = Signal()

    def _refresh_markers(self,cutout):
        self._displayables_by_cutout[cutout].clear_markers()
        self.mark_sources(self.current_cutout)

    def mark_apertures(self, cutout):
        logger.info("marking apertures on cutout.")
        x, y = cutout.pixel_source_point
        try:
            radii = (cutout.apcor.aperture, cutout.apcor.sky, cutout.apcor.swidth+cutout.apcor.sky)
        except:
            radii = (4, 15,30)
        self._displayables_by_cutout[cutout].place_annulus(x, y, radii, colour='r')

    def mark_sources(self, cutout):
        assert cutout in self._displayables_by_cutout

        x, y = cutout.pixel_source_point
        try:
            fwhm = float(cutout.astrom_header.get("FWHM",10))
        except:
            fwhm = 4.0
        radius = 2 * round(fwhm)

        colour = "b"
        if cutout.reading.from_input_file:
            if cutout.reading.null_observation:
                colour = "r"
            else:
                colour = "g"

        self._displayables_by_cutout[cutout].place_marker(x, y, radius,
                                                          colour=colour)

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

    def _create_displayable(self, cutout):
        return DisplayableImageSinglet(cutout.hdulist)

    def _attach_handlers(self, displayable):
        displayable.xy_changed.connect(self.xy_changed.fire)
        displayable.focus_released.connect(self.release_focus)

    def _detach_handlers(self, displayable):
        if displayable is not None:
            displayable.xy_changed.disconnect(self.xy_changed.fire)
            displayable.focus_released.disconnect(self.release_focus)

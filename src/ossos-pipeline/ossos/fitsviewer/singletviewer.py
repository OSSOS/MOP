__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.displayable import DisplayableImageSinglet
from ossos.fitsviewer.interaction import Signal


class SingletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent, canvas):
        super(SingletViewer, self).__init__(parent, canvas)

        self.current_cutout = None
        self.xy_changed = Signal()

        self._displayables_by_cutout = {}

    def display(self, cutout, mark_source=True):
        if cutout in self._displayables_by_cutout:
            displayable = self._displayables_by_cutout[cutout]
        else:
            displayable = DisplayableImageSinglet(cutout.hdulist)
            self._displayables_by_cutout[cutout] = displayable

        if self.current_displayable is not None:
            self.current_displayable.xy_changed.disconnect(self.xy_changed.fire)
            self.current_displayable.focus_released.disconnect(self.release_focus)

        self.current_cutout = cutout
        self.current_displayable = displayable

        self.current_displayable.xy_changed.connect(self.xy_changed.fire)
        self.current_displayable.focus_released.connect(self.release_focus)

        self._do_render(self.current_displayable)

        if mark_source:
            self.mark_source(cutout)

    def _do_render(self, displayable):
        displayable.render(self.canvas)

    def refresh_markers(self):
        self.mark_source(self.current_cutout)

    def mark_source(self, cutout):
        assert cutout in self._displayables_by_cutout

        x, y = cutout.pixel_source_point
        fwhm = float(cutout.astrom_header["FWHM"])
        radius = 2 * round(fwhm)
        self._displayables_by_cutout[cutout].place_marker(x, y, radius)

    def draw_error_ellipse(self, x, y, a, b, pa, redraw=True):
        """
        Draws an ErrEllipse with the spcified dimensions.  Only one ErrEllipse can be drawn and
        only once (not movable).
        """
        self.current_image.place_error_ellipse(x, y, a, b, pa)
        if redraw:
            self.redraw()

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

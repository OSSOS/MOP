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

        self._displayed_cutouts = {}

    def display(self, cutout):
        if cutout in self._displayed_cutouts:
            displayable = self._displayed_cutouts[cutout]
        else:
            displayable = DisplayableImageSinglet(cutout.hdulist)

        if self.current_cutout is not None:
            self.current_cutout.xy_changed.disconnect(self.xy_changed.fire)
            self.current_cutout.focus_released.disconnect(self.release_focus)

        self.current_cutout = displayable
        self.current_cutout.xy_changed.connect(self.xy_changed.fire)
        self.current_cutout.focus_released.connect(self.release_focus)

        self.current_cutout.render(self.canvas)

        self.mark_source(cutout)

    def refresh_markers(self):
        # TODO
        pass

    def mark_source(self, cutout):
        x, y = cutout.pixel_source_point
        fwhm = float(cutout.reading.get_observation_header()["FWHM"])
        radius = 2 * round(fwhm)
        self.current_cutout.place_marker(x, y, radius)

    def reset_colormap(self):
        if self.current_cutout is not None:
            self.current_cutout.reset_colormap()

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

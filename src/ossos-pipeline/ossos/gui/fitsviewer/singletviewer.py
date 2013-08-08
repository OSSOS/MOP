__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.gui.fitsviewer.displayable import DisplayableImageSinglet
from ossos.gui.fitsviewer.interaction import Signal


class SingletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent):
        super(SingletViewer, self).__init__(parent)

        self.current_image = None
        self.xy_changed = Signal()

        self._viewed_images = {}

    def display(self, fits_image, redraw=True):
        if fits_image in self._viewed_images:
            displayable = self._viewed_images[fits_image]
        else:
            displayable = DisplayableImageSinglet(fits_image.as_hdulist())
            self._viewed_images[fits_image] = displayable

        self.current_image = displayable

        displayable.display_changed.connect(self.redraw)
        displayable.xy_changed.connect(self.xy_changed.fire)
        displayable.focus_released.connect(self.release_focus)
        displayable.render(self.canvas)

        if redraw:
            self.redraw()

    def draw_circle(self, x, y, radius, redraw=True):
        """
        Draws a circle with the specified dimensions.  Only one circle can
        be on the image at a time, so any existing circle will be replaced.
        """
        self.current_image.draw_circle(x, y, radius)

        if redraw:
            self.redraw()

    def reset_colormap(self):
        if self.current_image is not None:
            self.current_image.reset_colormap()

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

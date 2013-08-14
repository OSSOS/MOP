__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.interaction import Signal


class SingletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent):
        super(SingletViewer, self).__init__(parent)

        self.current_image = None
        self.xy_changed = Signal()

    def display(self, displayable, redraw=True):
        if self.current_image is not None:
            self.current_image.display_changed.disconnect(self.redraw)
            self.current_image.xy_changed.disconnect(self.xy_changed.fire)
            self.current_image.focus_released.disconnect(self.release_focus)

        self.current_image = displayable
        self.current_image.display_changed.connect(self.redraw)
        self.current_image.xy_changed.connect(self.xy_changed.fire)
        self.current_image.focus_released.connect(self.release_focus)

        self.current_image.render(self.canvas)

        if redraw:
            self.redraw()

    def draw_marker(self, x, y, radius, redraw=True):
        """
        Draws a marker with the specified dimensions.  Only one marker can
        be on the image at a time, so any existing marker will be replaced.
        """
        self.current_image.place_marker(x, y, radius)

        if redraw:
            self.redraw()

    def reset_colormap(self):
        if self.current_image is not None:
            self.current_image.reset_colormap()

    def register_xy_changed_event_handler(self, handler):
        self.xy_changed.connect(handler)

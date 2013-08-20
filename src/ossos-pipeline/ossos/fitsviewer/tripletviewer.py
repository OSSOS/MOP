__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.displayable import DisplayableImageTriplet


class TripletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent, canvas):
        super(TripletViewer, self).__init__(parent, canvas)

        self.current_grid = None
        self._displayed_grids = {}

    def display(self, cutout_grid, redraw=True):
        if cutout_grid in self._displayed_grids:
            displayable = self._displayed_grids[cutout_grid]
        else:
            displayable = DisplayableImageTriplet(cutout_grid)

        self.current_grid = displayable
        self.current_grid.render(self.canvas)

        if redraw:
            self.redraw()

    def draw_marker(self, x, y, radius, redraw=True):
        """
        Draws a marker with the specified dimensions.  Only one marker can
        be on the image at a time, so any existing marker will be replaced.
        """
        self.current_grid.place_marker(x, y, radius)

        if redraw:
            self.redraw()

    def reset_colormap(self):
        if self.current_grid is not None:
            self.current_grid.reset_colormap()

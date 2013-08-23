__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.displayable import DisplayableImageTriplet


class TripletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

    def __init__(self, parent, canvas):
        super(TripletViewer, self).__init__(parent, canvas)

        self._displayables_by_grid = {}

    def display(self, cutout_grid):
        if cutout_grid in self._displayables_by_grid:
            displayable = self._displayables_by_grid[cutout_grid]
        else:
            displayable = DisplayableImageTriplet(cutout_grid)
            self._displayables_by_grid[cutout_grid] = displayable

        self.current_displayable = displayable
        self.current_displayable.render(self.canvas)

        self.mark_sources(cutout_grid)

    def refresh_markers(self):
        self.mark_sources(self.current_displayable.cutout_grid)

    def mark_sources(self, cutout_grid):
        for frame_index in range(cutout_grid.num_frames):
            self.mark_frame(cutout_grid, frame_index)

    def mark_frame(self, cutout_grid, frame_index):
        focus_cutout = cutout_grid.get_cutout(frame_index, frame_index)

        for time_index in range(cutout_grid.num_times):
            cutout = cutout_grid.get_cutout(frame_index, time_index)

            x, y = focus_cutout.pixel_source_point
            offset_x, offset_y = focus_cutout.reading.get_coordinate_offset(cutout.reading)
            x += offset_x
            y += offset_y

            fwhm = float(cutout.reading.get_observation_header()["FWHM"])
            radius = 2 * round(fwhm)

            self.current_displayable.get_singlet(frame_index, time_index).place_marker(x, y, radius)

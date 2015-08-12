__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.baseviewer import WxMPLFitsViewer
from ossos.fitsviewer.displayable import DisplayableImageTriplet


class TripletViewer(WxMPLFitsViewer):
    """
    Displays a single FITS image at a time.
    """

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

    def _create_displayable(self, cutout_grid):
        return DisplayableImageTriplet(cutout_grid, self.ds9)

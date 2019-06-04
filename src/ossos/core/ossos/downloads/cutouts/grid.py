__author__ = "David Rusk <drusk@uvic.ca>"


class CutoutGrid(object):
    """
    A grid of cutouts with frames being rows and times being columns.
    """

    def __init__(self, source):
        self.source = source
        self.num_frames = source.num_readings()
        self.num_times = source.num_readings()

        self._grid = [[None] * self.num_frames for _ in range(self.num_times)]

    @property
    def shape(self):
        return self.num_frames, self.num_times

    def apply(self, function):
        for frame_index in range(self.num_frames):
            for time_index in range(self.num_times):
                function(self.get_cutout(frame_index, time_index),
                         frame_index, time_index)

    def add_cutout(self, cutout, frame_index, time_index):
        self._grid[frame_index][time_index] = cutout

    def get_cutout(self, frame_index, time_index):
        return self._grid[frame_index][time_index]

    def get_hdulist(self, frame_index, time_index):
        return self.get_cutout(frame_index, time_index).hdulist

    def get_hdulists(self, frame_index):
        return [cutout.hdulist for cutout in self._grid[frame_index]]

    def is_filled(self):
        return all(element is not None
                   for frame in self._grid for element in frame)

    def reset_source_location(self):
        def reset_cutout(cutout, frame_index, time_index):
            cutout.reset_source_location()

        self.apply(reset_cutout)

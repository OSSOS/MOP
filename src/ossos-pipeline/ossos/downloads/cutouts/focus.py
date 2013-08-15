__author__ = "David Rusk <drusk@uvic.ca>"

import math


class FocalPointCalculator(object):
    def _convert_source_location(self, source_reading, reference_reading):
        """
        Converts the source (x, y) location from reading into the coordinate
        system of reference_reading.
        """
        offset_x, offset_y = reference_reading.get_coordinate_offset(source_reading)
        return source_reading.x + offset_x, source_reading.y + offset_y


class SingletFocalPointCalculator(FocalPointCalculator):
    """
    Calculates focal points for display as single images.  The focal point
    will be the location of the source in the middle observation.
    """

    def __init__(self, source):
        self.source = source

    def calculate_focal_point(self, reading):
        """
        Determines what the focal point of the downloaded image should be.

        Returns:
          focal_point: (x, y)
            The location of the source in the middle observation, in the
            coordinate system of the current source reading.
        """
        middle_index = int(math.ceil((len(self.source.get_readings()) / 2)))
        middle_reading = self.source.get_reading(middle_index)

        return self._convert_source_location(middle_reading, reading)


class TripletFocalPointCalculator(FocalPointCalculator):
    """
    Calculates the focal points for displaying triplets.
    """

    def __init__(self, source):
        self.source = source

    def calculate_focal_point(self, reading, frame_index):
        return self._convert_source_location(
            reading, self.source.get_reading(frame_index))

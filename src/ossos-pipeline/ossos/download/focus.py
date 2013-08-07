__author__ = "David Rusk <drusk@uvic.ca>"

import math


class FocalPointCalculator(object):
    def calculate_focal_points(self, source):
        """
        Determines all focal points of interest for a source.

        Returns:
          focal_points: list(FocalPoint)
        """
        focal_points = []
        for reading in source.get_readings():
            point = FocalPoint(reading,
                               self.calculate_focal_point(reading, source))
            focal_points.append(point)
        return focal_points

    def calculate_focal_point(self, reading, source):
        """
        Determines what the focal point of the downloaded image should be.

        Returns:
          focal_point: (x, y)
            The location of the source in the middle observation, in the
            coordinate system of the current source reading.
        """
        middle_index = int(math.ceil((len(source.get_readings()) / 2)))
        middle_reading = source.get_reading(middle_index)

        offset_x, offset_y = reading.get_coordinate_offset(middle_reading)

        return middle_reading.x + offset_x, middle_reading.y + offset_y


class FocalPoint(object):
    def __init__(self, reading, point):
        self.reading = reading
        self.point = point

from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        """

        @type y_offset: float
        @type x_offset: float
        """
        self.x_offset = x_offset
        self.y_offset = y_offset
        logger.debug("Convert initialized as dx,dy,inverted {},{}".format(x_offset, y_offset))

    def convert(self, point):
        """
        Convert a point from one coordinate system to another.

        Args:
          point: tuple(int x, int y)
            The point in the original coordinate system.

        Returns:
          converted_point: tuple(int x, int y)
            The point in the new coordinate system.

        Example: convert coordinate from original image into a pixel location
          within a cutout image.
        @type point: list(float,float)
        """
        x, y = point
        (x1, y1) = x - (self.x_offset - 1), y - (self.y_offset - 1)
        logger.debug("converted {} {} to {} {}".format(x, y, x1, y1))
        return x1, y1

    def get_inverse_converter(self):
        """
        Returns a converter object for converting back from this converter's
        output coordinate system to its input coordinate system.
        @rtype : CoordinateConverter
        """

        return CoordinateConverter(-self.x_offset, -self.y_offset)
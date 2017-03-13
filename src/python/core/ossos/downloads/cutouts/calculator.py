from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset, invert=False):
        """

        @type y_offset: float
        @type x_offset: float
        """
        self._dx = x_offset
        self._dy = y_offset
        self._invert = invert and -1 or 1
        logger.debug("Convert initialized as x_offset, y_offset {},{}".format(self.x_offset, self.y_offset))

    @property
    def x_offset(self):
        """
        Amount to subtract from x position to move to new reference frame.

        @rtype: float
        """
        return self._invert * self._dx

    @property
    def y_offset(self):
        """
        Amount to subtract from the y position to move to the new reference frame.

        @rtype: float
        """
        return self._invert * self._dy

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

        @rtype: list(float,float)
        """
        x, y = point
        (x1, y1) = x - self.x_offset, y - self.y_offset
        logger.debug("converted {} {} ==> {} {}".format(x, y, x1, y1))
        return x1, y1

    def get_inverse_converter(self):
        """
        Returns a converter object for converting back from this converter's
        output coordinate system to its input coordinate system.

        @rtype : CoordinateConverter
        """

        return CoordinateConverter(self.x_offset, self.y_offset, invert=True)
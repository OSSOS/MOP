from ossos.gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"


class CutoutCalculator(object):
    def __init__(self, slice_rows, slice_cols):
        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

    def build_cutout_str(self, extnum, focus, img_size, dx=0, dy=0, inverted=False):
        """
        Generates the cutout string needed for the vospace client's open
        with cutout feature.

        Args:
          extnum: str
            The extension number of the cutout.  See also the vospace service
            documentation at http://www.cadc.hia.nrc.gc.ca/data/
          focus: tuple(int x, int y)
            The x and y coordinates of the point which is the focus of the
            cutout.
          img_size: tuple(int xsize, int ysize)
            The size of the image being cutout from.
          inverted: bool
            Inverts the image before applying the cutout.  Defaults to False.

        Returns:
          cutout_str: str
            A string in the form [extnum][x0:x1,y0:y1]
          converter: CoordinateConverter
            Can be used to find a point in the sliced image based on its
            coordinate in the original image.
        """
        (x0, x1, y0, y1), converter = self.calc_cutout(focus, img_size, dx, dy, inverted)

        cutout_str = "[%s][%d:%d,%d:%d]" % (extnum, x0, x1, y0, y1)

        return cutout_str, converter

    def calc_cutout(self, focus, img_size, dx, dy, inverted=False ):
        """
        Calculates the start and stop points of the cutout around a point.

        Args:
          focus: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.
          img_size: tuple(int xsize, int ysize)
            The size of the image being cutout from.
          inverted: bool
            Inverts the image before applying the cutout.  Defaults to False.
        Returns:
            coords: (x0, x1, y0, y1)
              The cutout boundary coordinates
            converter: CoordinateConverter
              Can be used to find a point in the sliced image based on its
              coordinate in the original image.
        """
        x, y = focus
        img_size_x, img_size_y = img_size

        if inverted:
            x = img_size_x - x
            y = img_size_y - y

        x_mid_offset = max(dx,self.slice_cols) / 2
        y_mid_offset = max(dy,self.slice_rows) / 2

        xmin = max(1,x - x_mid_offset)
        xmax = min(img_size_x, x + x_mid_offset)
        ymin = max(1,y - y_mid_offset)
        ymax = min(img_size_y, y + y_mid_offset)

        # VOSpace cutout service only accepts integer values, so round
        # the values to the nearest int.

        def round_int(num):
            return int(round(num))

        xmin = round_int(xmin)
        xmax = round_int(xmax)
        ymin = round_int(ymin)
        ymax = round_int(ymax)

        if inverted:
            x0 = xmax
            x1 = xmin
            y0 = ymax
            y1 = ymin
            x_offset = img_size_x - xmax
            y_offset = img_size_y - ymax
        else:
            x0 = xmin
            x1 = xmax
            y0 = ymin
            y1 = ymax
            x_offset = xmin - 1
            y_offset = ymin - 1


        return (x0, x1, y0, y1), CoordinateConverter(x_offset, y_offset)

class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        self.x_offset = x_offset
        self.y_offset = y_offset
        logger.debug("Convert initialized as dx,dy,inverted {},{}".format(x_offset,
                                                                            y_offset))

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
        """
        x, y = point
        (x1, y1) =  x - self.x_offset, y - self.y_offset
        logger.debug("converted {} {} to {} {}".format(x,y, x1, y1))
        return x1, y1

    def get_inverse_converter(self):
        """
        Returns a converter object for converting back from this converter's
        output coordinate system to its input coordinate system.
        """
        return CoordinateConverter(-self.x_offset, -self.y_offset)
__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui import config
from ossos.gui import logger

from ossos.downloads.core import Downloader


class ImageCutoutDownloader(Downloader):
    """
    Downloads a slice of an image relevant to examining a (potential) source.
    """

    def __init__(self, slice_rows=None, slice_cols=None, vosclient=None):
        """
        Constructor.

        Args:
          slice_rows, slice_cols: int
            The number of rows and columns (pixels) to slice out around the
            source.  Leave as None to use default configuration values.
        """
        super(ImageCutoutDownloader, self).__init__(vosclient=vosclient)

        # If not provided, read defaults from application config file
        if slice_rows is None:
            slice_rows = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        if slice_cols is None:
            slice_cols = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")

        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def download_cutout(self, reading, focal_point):
        image_uri = reading.get_image_uri()

        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            reading.get_extension(),
            focal_point,
            reading.get_original_image_size(),
            inverted=reading.is_inverted())

        logger.debug("Calculated cutout: %s for %s"
                     % (cutout_str, image_uri))

        hdulist = self.download_hdulist(image_uri, view="cutout",
                                        cutout=cutout_str)

        return hdulist, converter


class CutoutCalculator(object):
    def __init__(self, slice_rows, slice_cols):
        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

    def build_cutout_str(self, extnum, point, img_size, inverted=False):
        """
        Generates the cutout string needed for the vospace client's open
        with cutout feature.

        Args:
          extnum: str
            The extension number of the cutout.  See also the vospace service
            documentation at http://www.cadc.hia.nrc.gc.ca/data/
          point: tuple(int x, int y)
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
        (x0, x1, y0, y1), converter = self.calc_cutout(point, img_size, inverted)

        cutout_str = "[%s][%d:%d,%d:%d]" % (extnum, x0, x1, y0, y1)

        return cutout_str, converter

    def calc_cutout(self, point, img_size, inverted=False):
        """
        Calculates the start and stop points of the cutout around a point.

        Args:
          point: (x, y)
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
        x, y = point
        img_size_x, img_size_y = img_size

        if inverted:
            x = img_size_x - x
            y = img_size_y - y

        x_mid_offset = self.slice_cols / 2
        y_mid_offset = self.slice_rows / 2

        xmin = x - x_mid_offset
        xmax = x + x_mid_offset
        ymin = y - y_mid_offset
        ymax = y + y_mid_offset

        # Make sure we don't try to slice outside the image boundaries
        if xmin < 1:
            diff = abs(xmin - 1)
            xmin += diff
            xmax += diff

        if ymin < 1:
            diff = abs(ymin - 1)
            ymin += diff
            ymax += diff

        if xmax > img_size_x:
            diff = abs(img_size_x - xmax)
            xmax -= diff
            xmin -= diff

        if ymax > img_size_y:
            diff = abs(img_size_y - ymax)
            ymax -= diff
            ymin -= diff

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
            x_offset = xmin
            y_offset = ymin

        return (x0, x1, y0, y1), CoordinateConverter(x_offset, y_offset)


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        self.x_offset = x_offset
        self.y_offset = y_offset

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
        return x - self.x_offset, y - self.y_offset

    def get_inverse_converter(self):
        """
        Returns a converter object for converting back from this converter's
        output coordinate system to its input coordinate system.
        """
        return CoordinateConverter(-self.x_offset, -self.y_offset)


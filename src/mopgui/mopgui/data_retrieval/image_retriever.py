"""
Retrieves slices of images relevant for display of sources to the user.
"""

import cStringIO

from astropy.io import fits
import vos


class ImageSliceRetriever(object):
    def __init__(self, slice_rows=256, slice_cols=256, vosclient=None):
        # TODO extract these to a application-wide config file
        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        self.vosclient = vos.Client() if vosclient is None else vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def retrieve_image(self, uri, source_reading):
        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        extension = str(int(source_reading.obs.ccdnum) + 1)

        # XXX have to be careful about boundary locations
        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            extension, source_reading.source_point)
        vofile = self.vosclient.open(uri, view="cutout", cutout=cutout_str)

        return fits.open(cStringIO.StringIO(vofile.read())), converter


class CutoutCalculator(object):
    def __init__(self, slice_rows, slice_cols):
        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

    def build_cutout_str(self, extnum, point):
        """
        Generates the cutout string needed for the vospace client's open
        with cutout feature.

        Args:
          extnum: str
            The extension number of the cutout.  See also the vospace service
            documentation at http://www.cadc.hia.nrc.gc.ca/data/
          point: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.

        Returns:
          cutout_str: str
            A string in the form [extnum][x0:x1,y0:y1]
          converter: CoordinateConverter
            Can be used to find a point in the sliced image based on its
            coordinate in the original image.
        """
        (x0, x1, y0, y1), converter = self.calc_cutout(point)

        cutout_str = "[%s][%d:%d,%d:%d]" % (extnum, x0, x1, y0, y1)

        return cutout_str, converter

    def calc_cutout(self, point):
        """
        Calculates the start and stop points of the cutout around a point.

        Args:
          point: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.
        Returns:
            coords: (x0, x1, y0, y1)
              The cutout boundary coordinates
            converter: CoordinateConverter
              Can be used to find a point in the sliced image based on its
              coordinate in the original image.
        """
        x, y = point

        x_mid_offset = self.slice_cols / 2
        y_mid_offset = self.slice_rows / 2

        coords = (x - x_mid_offset, x + x_mid_offset,
                  y - y_mid_offset, y + y_mid_offset)

        return coords, CoordinateConverter(x - x_mid_offset, y - y_mid_offset)


class CoordinateConverter(object):
    def __init__(self, x_offset, y_offset):
        self.x_offset = x_offset
        self.y_offset = y_offset

    def convert(self, point):
        x, y = point
        return x - self.x_offset, y - self.y_offset

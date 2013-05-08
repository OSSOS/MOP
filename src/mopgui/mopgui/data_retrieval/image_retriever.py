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
        cutout_str = self.cutout_calculator.build_cutout_str(
            extension, source_reading.source_point)
        vofile = self.vosclient.open(uri, view="cutout", cutout=cutout_str)

        return fits.open(cStringIO.StringIO(vofile.read()))


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
        """
        x0, x1, y0, y1 = self.calc_cutout(point)
        return "[%s][%d:%d,%d:%d]" % (extnum, x0, x1, y0, y1)

    def calc_cutout(self, point):
        """
        Calculates the start and stop points of the cutout around a point.

        Args:
          point: (x, y)
            The x and y coordinates of the point which is the focus of the
            cutout.
        Returns:
            coords: (x0, x1, y0, y1)
              The cutout boundary coordinates.
        """
        x = float(point[0])
        y = float(point[1])

        return (x - self.slice_cols / 2,
                x + self.slice_cols / 2,
                y - self.slice_rows / 2,
                y + self.slice_rows / 2)

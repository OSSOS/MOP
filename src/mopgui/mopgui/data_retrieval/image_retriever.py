"""
Retrieves slices of images relevant for display of sources to the user.
"""


class ImageSliceRetriever(object):
    def __init__(self):
        # TODO extract these to a application-wide config file
        self.slice_rows = 256
        self.slice_cols = 256

    def retrieve_image(self, uri, source_reading):
        # TODO:
        # use vos.Client open method with cutout parameter
        # XXX have to be careful about boundary locations
        pass


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
        return (point[0] - self.slice_cols / 2,
                point[0] + self.slice_cols / 2,
                point[1] - self.slice_rows / 2,
                point[1] + self.slice_rows / 2)

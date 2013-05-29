__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO
import tempfile

from astropy.io import fits


class FitsImage(object):
    """
    Provides the MOP's abstraction of a FITS file image.
    """

    def __init__(self, coord_converter):
        assert coord_converter is not None, "Must have a coordinate converter"

        self._coord_converter = coord_converter

    def get_pixel_coordinates(self, point):
        """
        Retrieves the pixel location of a point within the image given the
        location in the original FITS image.  This takes into account that
        the image may be a cutout of a larger original.

        Args:
          point: tuple(float, float)
            (x, y) in original.

        Returns:
          (x, y) pixel in this image.
        """
        return self._coord_converter.convert(point)

    def as_hdulist(self):
        raise NotImplementedError()

    def as_file(self):
        raise NotImplementedError()

    def close(self):
        raise NotImplementedError()


class InMemoryFitsImage(FitsImage):
    """
    Stores a FITS image file in memory without needing to write to disk.
    """

    def __init__(self, rawdata, coord_converter):
        """
        Constructs a new InMemoryFitsImage.

        Args:
          rawdata: str
        """
        super(InMemoryFitsImage, self).__init__(coord_converter)

        self._hdulist = fits.open(cStringIO.StringIO(rawdata))

    def as_hdulist(self):
        return self._hdulist

    def close(self):
        self._hdulist.close()


class InFileFitsImage(FitsImage):
    """
    Stores a FITS image file on disk using a temporary file.
    """

    def __init__(self, rawdata, coord_converter):
        """
        Constructs a new InFileFitsImage.

        Args:
          rawdata: str
        """
        super(InFileFitsImage, self).__init__(coord_converter)

        self._tempfile = tempfile.NamedTemporaryFile(mode="r+b", suffix=".fits")
        self._tempfile.write(rawdata)
        self._tempfile.flush()
        self._tempfile.seek(0)

        self._hdulist = None

    def as_hdulist(self):
        if self._hdulist is None:
            self._hdulist = fits.open(self._tempfile)

        return self._hdulist

    def close(self):
        if self._hdulist is not None:
            self._hdulist.close()

        self._tempfile.close()

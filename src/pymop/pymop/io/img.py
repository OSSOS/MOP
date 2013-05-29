__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO
import tempfile

from astropy.io import fits


class FitsImage(object):
    """
    Provides the MOP's abstraction of a FITS file image.
    """

    def __init__(self, coord_converter):
        self._coord_converter = coord_converter

    def as_hdulist(self):
        raise NotImplementedError()

    def close(self):
        raise NotImplementedError()


class InMemoryFitsImage(FitsImage):
    """
    Stores a FITS image file in memory without needing to write to disk.
    """

    def __init__(self, rawdata):
        """
        Constructs a new InMemoryFitsImage.

        Args:
          rawdata: str
        """
        self._hdulist = fits.open(cStringIO.StringIO(rawdata))

    def as_hdulist(self):
        return self._hdulist

    def close(self):
        self._hdulist.close()


class InFileFitsImage(FitsImage):
    """
    Stores a FITS image file on disk using a temporary file.
    """

    def __init__(self, rawdata):
        """
        Constructs a new InFileFitsImage.

        Args:
          rawdata: str
        """
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
        self._hdulist.close()
        self._tempfile.close()

__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO
import math
import tempfile

from astropy.io import fits


# Images from CCDs < 18 have their coordinates flipped
MAX_INVERTED_CCD = 17


class DownloadableItem(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self, reading, focal_point, needs_apcor, on_finished_callback,
                 in_memory=True):
        """
        Constructor.

        Args:
          source_reading: ossos.astrom.SourceReading
            The reading which will be the focus of the downloaded image.
          focal_point: tuple(int, int)
            The x, y coordinates that should be the focus of the downloaded
            image.  These coordinates should be in terms of the
            source_reading parameter's coordinate system.
          needs_apcor: bool
            If True, the apcor file with data needed for photometry
            calculations is downloaded in addition to the image.
          in_memory: bool
            If True, the image is stored in memory without being written to
            disk.  If False, the image will be written to a temporary file.
        """
        self.reading = reading
        self.focal_point = focal_point
        self.needs_apcor = needs_apcor
        self.on_finished_callback = on_finished_callback
        self.in_memory = in_memory

    def get_image_uri(self):
        return self.reading.get_image_uri()

    def get_apcor_uri(self):
        return self.reading.get_apcor_uri()

    def get_focal_point(self):
        """
        Determines what the focal point of the downloaded image should be.

        Returns:
          focal_point: (x, y)
            The location of the source in the middle observation, in the
            coordinate system of the current source reading.
        """
        return self.focal_point

    def get_full_image_size(self):
        """
        Returns:
          tuple(int width, int height)
            The full pixel size of the image before any cutouts.
        """
        return self.reading.get_original_image_size()

    def get_extension(self):
        """
        Returns:
          extension: str
            The FITS file extension to be downloaded.
        """
        if self._is_observation_fake():
            # We get the image from the CCD directory and it is not
            # multi-extension.
            return 0

        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        return str(self.get_ccd_num() + 1)

    def is_inverted(self):
        """
        Returns:
          inverted: bool
            True if the stored image is inverted.
        """
        if self._is_observation_fake():
            # We get the image from the CCD directory and it has already
            # been corrected for inversion.
            return False

        return True if self.get_ccd_num() <= MAX_INVERTED_CCD else False

    def get_ccd_num(self):
        """
        Returns:
          ccdnum: int
            The number of the CCD that the image is on.
        """
        return int(self.reading.get_observation().ccdnum)

    def finished_download(self, downloaded_item):
        """
        Triggers callbacks indicating the item has been downloaded.
        """
        self.on_finished_callback(self.reading, downloaded_item)

    def _is_observation_fake(self):
        return self.reading.get_observation().is_fake()


class DownloadedFitsImage(object):
    """
    A FITS file image which has been downloaded along with its apcor file.
    """

    def __init__(self, fits_str, coord_converter, apcor_str=None,
                 in_memory=True):
        """
        Constructs a new FitsImage object.

        Args:
          fits_str: str
            Raw data read from a FITS file in string format.
          coord_converter: ossos.cutouts.CoordinateConverter
            Converts coordinates from the original FITS file into pixel
            locations.  Takes into account cutouts.
          apcor_str: str:
            Raw data from from the .apcor file associated with this image.
            Defaults to None, in which case attempting to perform
            astrometric calculations will raise a ValueError.
          in_memory: bool
            If True, the FITS file will only be held in memory without
            writing to disk.  If False, the data will be written to a
            temporary file on disk and not held in memory.
            NOTE: calling as_hdulist will load the data into memory if
            it is only on disk.  Likewise, calling as_file on an "in memory"
            image will cause it to be written to disk.  Therefore this
            parameter is mostly for specifying the PREFERRED way of storing
            the data, not the only way in which it may be stored.
        """
        assert fits_str is not None, "No fits data"
        assert coord_converter is not None, "Must have a coordinate converter"

        self._coord_converter = coord_converter

        if apcor_str is not None:
            self._apcordata = ApcorData.from_raw_string(apcor_str)
        else:
            self._apcordata = None

        self._hdulist = None
        self._tempfile = None

        if in_memory:
            self._hdulist = self._create_hdulist(fits_str)
        else:
            self._tempfile = self._create_tempfile(fits_str)

        self._raw_fits_data = fits_str

    def _create_hdulist(self, strdata):
        return fits.open(cStringIO.StringIO(strdata))

    def _create_tempfile(self, strdata=None):
        tf = tempfile.NamedTemporaryFile(mode="r+b", suffix=".fits")

        if strdata is not None:
            tf.write(strdata)
            tf.flush()
            tf.seek(0)

        return tf

    def has_apcord_data(self):
        return self._apcordata is not None

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

    def get_observed_coordinates(self, point):
        """
        Retrieves the location of a point using the coordinate system of
        the original observation, i.e. the original image before any
        cutouts were done.

        Args:
          point: tuple(float, float)
            The pixel coordinates.

        Returns:
          (x, y) in the original image coordinate system.
        """
        return self._coord_converter.get_inverse_converter().convert(point)

    def as_hdulist(self):
        if self._hdulist is None:
            # we are currently storing "in file" only
            assert self._tempfile is not None

            self._tempfile.seek(0)
            self._hdulist = self._create_hdulist(self._tempfile.read())
            self._tempfile.seek(0)

        return self._hdulist

    def as_file(self):
        if self._tempfile is None:
            # we are currently storing "in memory" only
            assert self._hdulist is not None

            self._tempfile = self._create_tempfile()
            self._hdulist.writeto(self._tempfile.name)

        return self._tempfile

    def get_apcor_data(self):
        return self._apcordata

    def get_fits_header(self):
        return self.as_hdulist()[0].header

    def save(self, path):
        with open(path, "wb") as filehandle:
            filehandle.write(self._raw_fits_data)

    def close(self):
        if self._hdulist is not None:
            self._hdulist.close()
        if self._tempfile is not None:
            self._tempfile.close()


class ApcorData(object):
    def __init__(self, ap_in, ap_out, apcor, apcor_err):
        self.ap_in = ap_in
        self.ap_out = ap_out
        self.apcor = apcor
        self.apcor_err = apcor_err

    @classmethod
    def from_raw_string(cls, rawstr):
        """
        Creates an ApcorData record from the raw string format.

        Expected string format:
        ap_in ap_out   ap_cor  apcor_err
        """
        args = map(float, rawstr.split())
        return cls(*args)

    @property
    def aperture(self):
        return self.ap_in

    @property
    def sky(self):
        return self.ap_out + 1

    @property
    def swidth(self):
        return self.ap_in

__author__ = "David Rusk <drusk@uvic.ca>"

import cStringIO
import tempfile

from astropy.io import fits

from ossos import astrom
from ossos import wcs


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


class ImageReading(object):
    """
    Associates a particular source reading with a downloaded FITS image.
    """

    def __init__(self, reading, fits_image):
        self.reading = reading
        self._fits_image = fits_image

        self.original_observed_x = self.reading.x
        self.original_observed_y = self.reading.y

        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y

        self.pixel_x, self.pixel_y = self.get_pixel_location(
            self.observed_source_point)

        self._ra = self.reading.ra
        self._dec = self.reading.dec

        self._stale = False
        self._adjusted = False

    @property
    def observed_source_point(self):
        return self.observed_x, self.observed_y

    @property
    def pixel_source_point(self):
        return self.pixel_x, self.pixel_y

    def update_pixel_location(self, new_pixel_location):
        self.pixel_x, self.pixel_y = new_pixel_location
        self.observed_x, self.observed_y = self.get_observed_location(
            new_pixel_location)

        self._stale = True
        self._adjusted = True

    def reset_source_location(self):
        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y
        self.pixel_x, self.pixel_y = self.get_pixel_location(self.observed_source_point)

        self._stale = True
        self._adjusted = False

    @property
    def ra(self):
        self._lazy_refresh()
        return self._ra

    @property
    def dec(self):
        self._lazy_refresh()
        return self._dec

    def get_image(self):
        return self._fits_image

    def is_adjusted(self):
        return self._adjusted

    def get_pixel_location(self, observed_point):
        return self._fits_image.get_pixel_coordinates(observed_point)

    def get_observed_location(self, pixel_point):
        return self._fits_image.get_observed_coordinates(pixel_point)

    def get_observed_magnitude(self):
        if not self._fits_image.has_apcord_data():
            raise ValueError("Apcor data is required in order to calculate "
                             "observed magnitude.")

        # NOTE: this import is only here so that we don't load up IRAF
        # unnecessarily (ex: for candidates processing).
        from ossos import daophot

        apcor_data = self._fits_image.get_apcor_data()
        maxcount = float(self.reading.get_observation_header()["MAXCOUNT"])
        return daophot.phot_mag(self._fits_image.as_file().name,
                                self.pixel_x, self.pixel_y,
                                aperture=apcor_data.aperture,
                                sky=apcor_data.sky,
                                swidth=apcor_data.swidth,
                                apcor=apcor_data.apcor,
                                maxcount=maxcount)

    def _lazy_refresh(self):
        if self._stale:
            self._update_ra_dec()
            self._stale = False

    def _update_ra_dec(self):
        astrom_header = self.reading.get_observation_header()
        fits_header = self.get_image().get_fits_header()

        self._ra, self._dec = wcs.xy2sky(self.observed_x, self.observed_y,
                                         float(astrom_header[astrom.CRPIX1]),
                                         float(astrom_header[astrom.CRPIX2]),
                                         float(astrom_header[astrom.CRVAL1]),
                                         float(astrom_header[astrom.CRVAL2]),
                                         wcs.parse_cd(fits_header),
                                         wcs.parse_pv(fits_header),
                                         wcs.parse_order_fit(fits_header))


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


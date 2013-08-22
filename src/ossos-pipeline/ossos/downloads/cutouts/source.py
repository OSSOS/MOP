__author__ = "David Rusk <drusk@uvic.ca>"

import tempfile

from ossos import wcs, astrom


class SourceCutout(object):
    """
    A cutout around a source.
    """

    def __init__(self, reading, hdulist, coordinate_converter, apcor=None):
        self.reading = reading
        self.hdulist = hdulist
        self.coordinate_converter = coordinate_converter
        self.apcor = apcor

        self.original_observed_x = self.reading.x
        self.original_observed_y = self.reading.y

        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y

        self.pixel_x, self.pixel_y = self.get_pixel_coordinates(
            self.observed_source_point)

        self._ra = self.reading.ra
        self._dec = self.reading.dec

        self._stale = False
        self._adjusted = False

        self._tempfile = None

    @property
    def observed_source_point(self):
        return self.observed_x, self.observed_y

    @property
    def pixel_source_point(self):
        return self.pixel_x, self.pixel_y

    def update_pixel_location(self, new_pixel_location):
        self.pixel_x, self.pixel_y = new_pixel_location
        self.observed_x, self.observed_y = self.get_observed_coordinates(
            new_pixel_location)

        self._stale = True
        self._adjusted = True

    def reset_source_location(self):
        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y
        self.pixel_x, self.pixel_y = self.get_pixel_coordinates(
            self.observed_source_point)

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

    def get_fits_header(self):
        return self.hdulist[0].header

    def is_adjusted(self):
        return self._adjusted

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
        return self.coordinate_converter.convert(point)

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
        return self.coordinate_converter.get_inverse_converter().convert(point)

    def get_observed_magnitude(self):
        if self.apcor is None:
            raise ValueError("Apcor data is required in order to calculate "
                             "observed magnitude.")

        # NOTE: this import is only here so that we don't load up IRAF
        # unnecessarily (ex: for candidates processing).
        from ossos import daophot

        maxcount = float(self.reading.get_observation_header()["MAXCOUNT"])
        return daophot.phot_mag(self._hdulist_on_disk(),
                                self.pixel_x, self.pixel_y,
                                aperture=self.apcor.aperture,
                                sky=self.apcor.sky,
                                swidth=self.apcor.swidth,
                                apcor=self.apcor.apcor,
                                maxcount=maxcount)

    def _hdulist_on_disk(self):
        """
        IRAF routines such as daophot need input on disk.

        Returns:
          filename: str
            The name of the file containing the FITS data.
        """
        if self._tempfile is None:
            self._tempfile = tempfile.NamedTemporaryFile(
                mode="r+b", suffix=".fits")
            self.hdulist.writeto(self._tempfile.name)

        return self._tempfile.name

    def _lazy_refresh(self):
        if self._stale:
            self._update_ra_dec()
            self._stale = False

    def _update_ra_dec(self):
        fits_header = self.get_fits_header()

        self._ra, self._dec = wcs.xy2sky(self.pixel_x, self.pixel_y,
                                         float(fits_header[astrom.CRPIX1]),
                                         float(fits_header[astrom.CRPIX2]),
                                         float(fits_header[astrom.CRVAL1]),
                                         float(fits_header[astrom.CRVAL2]),
                                         wcs.parse_cd(fits_header),
                                         wcs.parse_pv(fits_header),
                                         wcs.parse_order_fit(fits_header))

import traceback
import tempfile

from astropy import units

from astropy.coordinates import SkyCoord

from astropy.io import fits

from ...astrom import SourceReading, Observation
from ...gui import logger
from ... import storage
from ossos.gui import config

__author__ = "David Rusk <drusk@uvic.ca>"


class SourceCutout(object):
    """
    A cutout around a source.
    """

    def __init__(self, reading, hdulist, apcor=None, zmag=None, radius=None):
        """
        :param reading: A source reading giving the measurement of the object associated with this cutout.
        :param hdulist: the HDUList containing the cutout.
        :param apcor: The aperture correction for this cutout.
        :param zmag: The zeropoint of the flux calibration for this cutout.
        :return:
        """
        logger.debug("building a SourceCutout.")
        assert isinstance(reading, SourceReading)
        assert isinstance(hdulist, fits.HDUList)
        # assert isinstance(apcor, ApcorData)

        self.radius = radius
        self.reading = reading
        self.hdulist = hdulist
        self.apcor = apcor
        self.zmag = zmag
        self.pixel_y = self.pixel_x = self._extno = None

        if self.reading.x is None or self.reading.y is None or (self.reading.x == -9999 and self.reading.y == -9999):
            (x, y) = self.hdulist[self.extno].wcs.sky2xy(self.reading.ra, self.reading.dec)
            self.reading.pix_coord = hdulist[self.extno].converter.get_inverse_converter().convert((x, y))
            self.reading.obs.ccdnum = self.extno - 1

        self.original_observed_ext = self.extno
        self.original_observed_x = self.reading.x
        self.original_observed_y = self.reading.y

        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y

        if self.pixel_x is None or self.pixel_y is None:
            self.pixel_x, self.pixel_y = self.get_pixel_coordinates(self.flip_flip(self.observed_source_point))

        self._ra = self.reading.ra * units.degree
        self._dec = self.reading.dec * units.degree
        self._ext = self.original_observed_ext

        self._stale = False
        self._adjusted = False
        self._comparison_image = None
        self._tempfile = None
        self._bad_comparison_images = [self.hdulist[-1].header.get('EXPNUM', None)]
        logger.debug("type X/Y {}/{}".format(type(self.pixel_x),
                                             type(self.pixel_y)))

    @property
    def extno(self):
        if self._extno is None:
            self._extno = SourceCutout.resolve_extno(self.hdulist, self.reading.get_ccd_num())
        return self._extno

    @staticmethod
    def resolve_extno(hdulist, ccdnum):
        for (extno, hdu) in enumerate(hdulist):
            if ccdnum == int(hdu.header.get('EXTVER', -1)):
                return extno
        return len(hdulist) - 1

    def flip_flip(self, point):
        """
        Sometimes we 'flipped' the X/Y coordinates of the image during processing.  This function takes an
        (x,y) location from the processing pipeline and returns the x/y location relative to the original
        reference frame.   (Correctly checks if the coordinate is flipped to begin with.)

        @param point: an x/y location on the image.
        @type point: [x, y]
        @return:  the x/y location on the un-fliped frame
        """
        x, y = point
        if self.reading.compute_inverted():
            naxis1 = self.reading.obs.header.get(self.reading.obs.HEADER_IMG_SIZE_X,
                                                 self.reading.obs.header.get('NAXIS1', 256))
            naxis2 = self.reading.obs.header.get(self.reading.obs.HEADER_IMG_SIZE_Y,
                                                 self.reading.obs.header.get('NAXIS2', 256))
            logger.debug("Got that image has size {}, {}".format(naxis1, naxis2))

            x1 = x
            y1 = y
            x = int(naxis1) - x + 1
            y = int(naxis2) - y + 1
            logger.debug("Inverted source location from {},{} to {},{}".format(x1, y1, x, y))

        return x, y

    @property
    def astrom_header(self):
        return self.hdulist[len(self.hdulist) - 1].header

    @property
    def fits_header(self):
        return self.hdulist[len(self.hdulist) - 1].header

    @property
    def observed_source_point(self):
        return self.observed_x, self.observed_y

    @property
    def pixel_source_point(self):
        return self.pixel_x, self.pixel_y

    def update_pixel_location(self, new_pixel_location, extno=1):
        self.pixel_x, self.pixel_y = new_pixel_location
        self.observed_x, self.observed_y = self.get_observed_coordinates(
            new_pixel_location, extno=extno)
        self._ext = extno
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

    def is_adjusted(self):
        return self._adjusted

    def get_pixel_coordinates(self, point, extno=None):
        """
        Retrieves the pixel location of a point within the current HDUList given the
        location in the original FITS image.  This takes into account that
        the image may be a cutout of a larger original.

        The approximate RA/DEC are required in-case there is more than one HDU for this source,
        that can happen when a source is near the pixel boundary.

        Args:
          point: tuple(float, float)
            (x, y) in original.

        Returns:
          (x, y) pixel in this image.
        """
        if not extno:
            extno = self.original_observed_ext
        return self.hdulist[extno].converter.convert(point)

    def get_observed_coordinates(self, point, extno=None):
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
        return self.hdulist[extno].converter.get_inverse_converter().convert(point)

    def pix2world(self, x, y, usepv=True):
        return self.hdulist[self.extno].wcs.xy2sky(x, y, usepv=usepv)

    def world2pix(self, ra, dec, usepv=True):
        """
        Convert a given RA/DEC position to the Extension X/Y location.

         Here we loop over the hdulist to find the extension is RA/DEC are from and then return the appropriate X/Y
        :param ra: The Right Ascension of the point
        :type ra: Quantity
        :param dec: The Declination of the point
        :type dec: Quantity
        :return: X, Y, Extension position of source in cutout reference frame
        :rtype: (float, float, int)
        """
        logger.debug("Converting (ra,dec) to (x,y) given {:8.3f} {:8.3f} for image {}".format(
            ra, dec, self.reading.get_exposure_number()))
        for idx in range(1, len(self.hdulist)):
            logger.debug("Trying convert using extension: {0}".format(idx))
            hdu = self.hdulist[idx]
            x, y = hdu.wcs.sky2xy(ra, dec, usepv=usepv)
            logger.debug("Tried converting RA/DEC {0},{1} to X/Y and got {2},{3}".format(ra, dec, x, y))
            if 0 < x < hdu.header['NAXIS1'] and 0 < y < hdu.header['NAXIS2']:
                logger.debug("Inside the frame.")
                return x, y, idx
        return None, None, None

    def get_observed_magnitude(self):
        # NOTE: this import is only here so that we don't load up IRAF
        # unnecessarily (ex: for candidates processing).
        """
        Get the magnitude at the current pixel x/y location.

        :return: Table
        """
        from ossos import daophot

        max_count = float(self.astrom_header.get("MAXCOUNT", 30000))
        tmp_file = self._hdulist_on_disk()
        if not self.zmag:
            self.zmag = self.hdulist[self._ext].header.get('PHOTZP', 0.0)
        try:
            phot = daophot.phot_mag(tmp_file,
                                    self.pixel_x, self.pixel_y,
                                    aperture=self.apcor.aperture,
                                    sky=self.apcor.sky,
                                    swidth=self.apcor.swidth,
                                    apcor=self.apcor.apcor,
                                    zmag=self.zmag,
                                    maxcount=max_count, extno=1)
            if not self.apcor.valid:
                phot['PIER'][0] = 1
            return phot
        finally:
            self.close()

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
            self.hdulist[self._ext].writeto(self._tempfile.name)
        return self._tempfile.name

    def close(self):
        """
        Once we are done with the on disk content we should close the filehandle.
        """
        if self._tempfile is not None:
            self._tempfile.close()
            self._tempfile = None

    def _lazy_refresh(self):
        if self._stale:
            self._update_ra_dec()
            self._stale = False

    def _update_ra_dec(self):
        hdu = self.hdulist[self.original_observed_ext]
        self._ra, self._dec = hdu.wcs.xy2sky(self.pixel_x, self.pixel_y)

    @property
    def comparison_image(self):
        if self._comparison_image is None:
            self.retrieve_comparison_image()
        return self._comparison_image

    def retrieve_comparison_image(self):
        """
        Search the DB for a comparison image for this cutout.
        """
        # selecting comparator when on a comparator should load a new one.

        ref_ra = self.reading.ra * units.degree
        ref_dec = self.reading.dec * units.degree
        radius = self.radius is not None and self.radius or config.read('CUTOUTS.SINGLETS.RADIUS') * units.arcminute
        query_result = storage.cone_search(ref_ra, ref_dec, radius, radius)  # returns an astropy.table.table.Table
        comparison = None
        if len(query_result['collectionID']) > 0:  # are there any comparison images even available on that sky?
            for collectionID in query_result['collectionID']:
                if collectionID not in self._bad_comparison_images:
                    comparison = collectionID
                    self._bad_comparison_images.append(comparison)
                    try:
                        hdu_list = storage.ra_dec_cutout(storage.dbimages_uri(comparison),
                                                         SkyCoord(ref_ra, ref_dec), radius)
                        obs = Observation(str(comparison), 'p',
                                          ccdnum=int(hdu_list[-1].header.get('EXTVER', 0)))
                        x = hdu_list[-1].header.get('NAXIS1', 0) // 2.0
                        y = hdu_list[-1].header.get('NAXIS2', 0) // 2.0
                        reading = SourceReading(x, y, self.reading.x, self.reading.y,
                                                ref_ra, ref_dec, self.reading.x, self.reading.y, obs)
                        self._comparison_image = SourceCutout(reading, hdu_list)
                    except Exception as ex:
                        logger.error("{} {}".format(type(ex), str(ex)))
                        logger.error(traceback.format_exc())
                        continue
                    break
            if comparison is None:
                logger.critical(str(self.fits_header))
                self._comparison_image = None
                return
        else:
            query_result.pprint()
            logger.info("No comparison images available for this piece of sky.")
            print "No comparison images available for this piece of sky."
            self._comparison_image = None
            return

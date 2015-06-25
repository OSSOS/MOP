import urllib
from astropy.io import fits
from ...astrom import SourceReading, Observation
from ...downloads.core import ApcorData
from ...gui import logger
import tempfile
from ... import wcs, storage

__author__ = "David Rusk <drusk@uvic.ca>"


class SourceCutout(object):
    """
    A cutout around a source.
    """

    def __init__(self, reading, hdulist, apcor=None, zmag=None):
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
        assert isinstance(apcor, ApcorData)

        self.reading = reading
        self.hdulist = hdulist
        self.apcor = apcor
        self.zmag = zmag
        self.original_observed_ext = None

        if self.reading.x is None or self.reading.y is None or (self.reading.x == -9999 and self.reading.y == -9999):
            for extno in range(1, len(self.hdulist)):
                hdu = self.hdulist[extno]
                try:
                    logger.debug("Converting {} {} to X/Y ".format(self.reading.ra, self.reading.dec))
                    (x, y) = hdu.wcs.sky2xy(self.reading.ra, self.reading.dec)
                    logger.debug("Got {} {} ".format(x, y))
                    if 0 < x < hdu.header.get('NAXIS1', 0) and 0 < y < hdu.header.get('NAXIS2', 0):
                        self.reading.pix_coord = hdu.converter.get_inverse_converter().convert((x, y))
                        self.original_observed_ext = extno
                        self.reading.obs.ccdnum = extno - 1
                        break
                except:
                    pass

        self.original_observed_x = self.reading.x
        self.original_observed_y = self.reading.y
        if self.original_observed_ext is None:
            self.original_observed_ext = len(self.hdulist) - 1


        self.observed_x = self.original_observed_x
        self.observed_y = self.original_observed_y

        self.pixel_x, self.pixel_y = self.get_pixel_coordinates(
            self.observed_source_point, self.original_observed_ext)

        self._ra = self.reading.ra
        self._dec = self.reading.dec
        self._ext = self.original_observed_ext

        self._stale = False
        self._adjusted = False
        self._comparison_image = None
        self._tempfile = None
        self._bad_comparison_images = [self.hdulist[-1].header.get('EXPNUM', None)]
        logger.error("type X/Y {}/{}".format(type(self.pixel_x),
                                             type(self.pixel_y)))

    @property
    def astrom_header(self):
        return self.hdulist[len(self.hdulist) - 1].header
        #return self.reading.get_observation_header()

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
            new_pixel_location, extno)
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

    def get_pixel_coordinates(self, point, extno=1):
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
        return self.hdulist[extno].converter.convert(point)

    def get_observed_coordinates(self, point, extno=1):
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

    def world2pix(self, ra, dec):
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
        for idx in range(1, len(self.hdulist)):
            hdu = self.hdulist[idx]
            x, y = hdu.wcs.wcs_world2pix(ra, dec, 1)
            if 0 < x < hdu.header['NAXIS1'] and 0 < y < hdu.header['NAXIS2']:
                return x, y, idx
        return None, None, None

    def get_observed_magnitude(self, **kwargs):
        # NOTE: this import is only here so that we don't load up IRAF
        # unnecessarily (ex: for candidates processing).
        from ossos import daophot

        max_count = float(self.astrom_header.get("MAXCOUNT", 30000))
        x, y, mag, merr = daophot.phot_mag(self._hdulist_on_disk(),
                                           self.pixel_x, self.pixel_y,
                                           aperture=self.apcor.aperture,
                                           sky=self.apcor.sky,
                                           swidth=self.apcor.swidth,
                                           apcor=self.apcor.apcor,
                                           zmag=self.zmag,
                                           maxcount=max_count)
        self.close()
        if not self.apcor.valid:
            mag = None
            merr = None

        return x, y, mag, merr

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
        fits_header = self.fits_header
        self._ra, self._dec = wcs.WCS(fits_header).xy2sky(self.pixel_x, self.pixel_y)
        logger.debug("computed RA/DEC using WCS {},{} -> {},{}".format(
            self.pixel_x, self.pixel_y,
            self._ra, self._dec))

    @property
    def comparison_image(self):
        return self._comparison_image

    def retrieve_comparison_image(self, downloader):
        """
        Search the DB for a comparison image for this cutout.
        """
        # selecting comparator when on a comparator should load a new one.

        ref_wcs = wcs.WCS(self.fits_header)
        try:
            ref_x = self.fits_header['NAXIS1'] / 2.0
            ref_y = self.fits_header['NAXIS2'] / 2.0
            (ref_ra, ref_dec) = ref_wcs.xy2sky(ref_x, ref_y)
        except Exception as e:
            logger.info(str(e))
            logger.info(str(self.fits_header))
            return None

        dra = self.fits_header['CD1_1'] * self.fits_header['NAXIS1'] / 2.0
        ddec = self.fits_header['CD2_2'] * self.fits_header['NAXIS2'] / 2.0
        radius = max(dra, ddec)

        logger.info("BOX({} {} {} {})".format(ref_ra, ref_dec, dra, ddec))

        query_result = storage.cone_search(ref_ra, ref_dec, dra, ddec)  # returns an astropy.table.table.Table

        comparison = None
        if len(query_result['collectionID']) > 0:  # are there any comparison images even available on that sky?
            for collectionID in query_result['collectionID']:
                if collectionID not in self._bad_comparison_images:
                    comparison = collectionID
                    self._bad_comparison_images.append(comparison)
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

        base_url = "https://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/vospace/nodes/OSSOS/dbimages/{}/{}p.fits".format(
            comparison, comparison)
        cutout = 'CIRCLE ICRS {} {} {}'.format(ref_ra, ref_dec, radius)
        url = base_url + "?" + urllib.urlencode({'view': 'cutout', 'cutout': cutout})

        hdu_list = downloader.download_hdulist(uri=None, URL=url)

        comp_wcs = wcs.WCS(hdu_list[-1].header)
        (x, y) = comp_wcs.sky2xy(ref_ra, ref_dec)
        obs = Observation(str(comparison), 'p', ccdnum=str(hdu_list[-1].header.get('EXTVER', 0)))
        reading = SourceReading(x, y, ref_x, ref_y, ref_ra, ref_dec, ref_x, ref_y, obs)
        self._comparison_image = SourceCutout(reading, hdu_list)

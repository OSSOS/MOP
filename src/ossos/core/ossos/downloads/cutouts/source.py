"""
The cutouts.source module contains SourceCutout class that provides access to the section of an image containing
the source.  The SourceCutout provides RA/DEC -> X/Y and X/Y -> RA/DEC mapping as well as cutout X/Y -> full image X/Y
"""

import traceback
import tempfile
from astropy import units
from astropy.coordinates import SkyCoord
from astropy.io import fits
from astropy.units import Quantity
from astropy.time import Time
from astropy.table import Table
import numpy


from ...astrom import SourceReading, Observation
from ...gui import logger
from ... import storage
from ossos.gui import config
from .downloader import Downloader, ApcorData

__author__ = "David Rusk <drusk@uvic.ca>"


class SourceCutout(object):
    """
    A cutout around a source.
    """

    def __init__(self, reading, hdulist, radius=None):
        """
        :param reading: A source reading giving the measurement of the object associated with this cutout.
        :param hdulist: the HDUList containing the cutout.
        :return:
        """
        logger.debug("building a SourceCutout.")
        assert isinstance(reading, SourceReading)
        assert isinstance(hdulist, fits.HDUList)

        self.radius = radius
        self.reading = reading
        self.hdulist = hdulist
        self._adjusted = False
        self._apcor = None
        self._zmag = None
        self.init_skycoord = reading.sky_coord

        if self.reading.x is None or self.reading.y is None or (self.reading.x == -9999 and self.reading.y == -9999):
            # indicates that reading assocated with this SourceCutout doesn't have x/y/extension information.
            # Here we derive that information from the RA/DEC and the cutout parameters.
            (x, y, extno) = self.world2pix(self.reading.ra, self.reading.dec)
            self.reading.pix_coord = self.get_observation_coordinates(x, y, extno)
            self.reading.obs.ccdnum = self.get_ccdnum(extno)

        self._comparison_image = []
        self._comparison_image_index = None
        self._comparison_image_list = None
        self._tempfile = None
        self._bad_comparison_images = [self.hdulist[-1].header.get('EXPNUM', None)]

    def reset_coord(self):
        """
        Reset the source location based on the init_skycoord values
        @return:
        """
        (x, y, idx) = self.world2pix(self.init_skycoord.ra,
                                     self.init_skycoord.dec,
                                     usepv=True)
        self.update_pixel_location((x, y), idx)

    @property
    def pixel_coord(self):
        """
        Return the coordinates of the source in the cutout reference frame.
        @return:
        """
        return self.get_pixel_coordinates(self.reading.pix_coord, self.reading.get_ccd_num())

    @property
    def pixel_x(self):
        """
        The X coordinate of the reading in the cutout reference frame.
        @return: int
        """
        return self.pixel_coord[0]

    @property
    def pixel_y(self):
        """
        The Y coordiante of the reading in the cutout reference frame.
        @return:
        """
        return self.pixel_coord[1]

    @property
    def extno(self):
        """
        Which extension of the cutout is the current reading located in.
        A cutout can have multiple extensions.  The reading gives an x/y which refers to a
        specific extension.   If extno is not set then we can resolve the correct one.

        @return: int
        """

        return self.get_hdulist_idx(self.reading.get_ccd_num())

    def get_ccdnum(self, hdulist_index):
        """
        Get the CCDNUM for a given hdulist idx.

        @param hdulist_index:  the index of the HDUList entry that the CCDNUM is needed for.
        @return: ccdnum
        """
        return self.hdulist[hdulist_index].header.get('EXTVER')

    def get_hdulist_idx(self, ccdnum):
        """
        The SourceCutout is a list of HDUs, this method returns the index of the HDU that corresponds to the given
        ccd number.  CCDs are numbers from 0, but the first CCD (CCDNUM=0) is often in extension 1 of an MEF.
        @param ccdnum: the number of the CCD in the MEF that is being referenced.
        @return: the index of in self.hdulist that corresponds to the given CCD number.
        """
        for (extno, hdu) in enumerate(self.hdulist):
            if ccdnum == int(hdu.header.get('EXTVER', -1)) or str(ccdnum) in hdu.header.get('AMPNAME', ''):
                return extno
        raise ValueError("Failed to find requested CCD Number {} in cutout {}".format(ccdnum,
                                                                                      self))

    def flip_flip(self, point):
        """
        Sometimes we 'flipped' the X/Y coordinates of the image during processing.  This function takes an
        (x,y) location from the processing pipeline and returns the x/y location relative to the original
        reference frame.   (Correctly checks if the coordinate is flipped to begin with.)

        This is only for use on the original X/Y coordinates as provided by an input reading.

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
        return self.reading.pix_coord

    @property
    def pixel_source_point(self):
        return self.pixel_coord

    def update_pixel_location(self, new_pixel_location, hdu_index):
        """
        Update the x/y location of the associated reading by using new_pixel_location and transforming from cutout
        reference frame to observation refrence frame.
        @param new_pixel_location: (x, y) location of the source in cutout pixel reference frame
        @param hdu_index: index of the associate hdu in the hdulist for this cutout
        """
        self.reading.pix_coord = self.get_observation_coordinates(new_pixel_location[0],
                                                                  new_pixel_location[1],
                                                                  hdu_index)
        new_ccd = self.get_ccdnum(hdu_index)
        if new_ccd != self.reading.get_ccd_num():
            self._apcor = None
            self._zmag = None
            self.reading.obs.ccdnum = self.get_ccdnum(hdu_index)
        self.reading.sky_coord = self.pix2world(new_pixel_location[0],
                                                new_pixel_location[1],
                                                hdu_index)

    @property
    def ra(self):
        """
        Return the RA source.
        @return:
        """
        return self.reading.ra

    @property
    def dec(self):
        return self.reading.dec

    def is_adjusted(self):
        return self._adjusted

    def get_pixel_coordinates(self, point, ccdnum):
        """
        Retrieves the pixel location of a point within the current HDUList given the
        location in the original FITS image.  This takes into account that
        the image may be a cutout of a larger original.

        Args:
          point: tuple(float, float)
            (x, y) in original.

        Returns:
          (x, y) pixel in this image.
          @param extno: the extno from the original Mosaic that the x/y coordinates are from.
        """
        hdulist_index = self.get_hdulist_idx(ccdnum)
        if isinstance(point[0], Quantity) and isinstance(point[1], Quantity):
            pix_point = point[0].value, point[1].value
        else:
            pix_point = point
        if self.reading.inverted:
            pix_point = self.reading.obs.naxis1 - pix_point[0] +1 , self.reading.obs.naxis2 - pix_point[1] + 1

        (x, y) = self.hdulist[hdulist_index].converter.convert(pix_point)
        return x, y, hdulist_index

    def get_observation_coordinates(self, x, y, hdulist_index):
        """
        Retrieves the location of a point using the coordinate system of
        the original observation, i.e. the original image before any
        cutouts were done.

        Returns:
          (x, y) in the original image coordinate system.
          @param x: x-pixel location in the cutout frame of reference
          @param y: y-pixel location in the cutout frame of reference
          @param idx: index of hdu in hdulist that the given x/y corresponds to.
        """
        return self.hdulist[hdulist_index].converter.get_inverse_converter().convert((x, y))

    def pix2world(self, x, y, hdulist_index, usepv=True):
        ra, dec = self.hdulist[hdulist_index].wcs.xy2sky([x], [y], usepv=usepv)
        return ra[0], dec[0]

    def world2pix(self, ra, dec, usepv=True):
        """
        Convert a given RA/DEC position to the  X/Y/HDULIST_INDEX in the cutout frame.

         Here we loop over the hdulist to find the extension is RA/DEC are from and then return the appropriate X/Y
        :param ra: The Right Ascension of the point
        :type ra: Quantity
        :param dec: The Declination of the point
        :type dec: Quantity
        :return: X, Y, Extension position of source in cutout reference frame
        :rtype: (float, float, int)
        @param usepv: Should the non-linear part of the WCS be using to compute the transformation? (default: True)
        """
        x, y, idx = (0, 0, 0)
        for idx in range(1, len(self.hdulist)):
            hdu = self.hdulist[idx]
            x, y = hdu.wcs.sky2xy(ra, dec, usepv=usepv)
            if 0 < x < hdu.header['NAXIS1'] and 0 < y < hdu.header['NAXIS2']:
                # print "Inside the frame."
                return x, y, idx
        return x, y, idx

    @property
    def zmag(self,):
        """
        Return the photometric zeropoint of the CCD associated with the reading.
        @return: float
        """
        if self._zmag is None:
            hdulist_index = self.get_hdulist_idx(self.reading.get_ccd_num())
            self._zmag = self.hdulist[hdulist_index].header.get('PHOTZP', 0.0)
        return self._zmag

    @property
    def apcor(self):
        """
        return the aperture correction of for the CCD assocated with the reading.
        @return: Apcor
        """
        if self._apcor is None:
            try:
                self._apcor = Downloader().download_apcor(self.reading.get_apcor_uri())
            except:
                self._apcor = ApcorData.from_string("5 15 99.99 99.99")
        return self._apcor

    def get_observed_magnitude(self, centroid=True):
        # NOTE: this import is only here so that we don't load up IRAF
        # unnecessarily (ex: for candidates processing).
        """
        Get the magnitude at the current pixel x/y location.

        :return: Table
        """

        max_count = float(self.astrom_header.get("MAXCOUNT", 30000))
        (x, y, hdulist_index) = self.pixel_coord
        tmp_file = self._hdu_on_disk(hdulist_index)
        try:
            from ossos import daophot
            phot = daophot.phot_mag(tmp_file,
                                    x, y,
                                    aperture=self.apcor.aperture,
                                    sky=self.apcor.sky,
                                    swidth=self.apcor.swidth,
                                    apcor=self.apcor.apcor,
                                    zmag=self.zmag,
                                    maxcount=max_count, extno=1,
                                    centroid=centroid)
            if not self.apcor.valid:
                phot['PIER'][0] = 1
            return phot
        except Exception as ex:
            print(ex)
            raise ex
        finally:
            self.close()

    def _hdu_on_disk(self, hdulist_index):
        """
        IRAF routines such as daophot need input on disk.

        Returns:
          filename: str
            The name of the file containing the FITS data.
        """
        if self._tempfile is None:
            self._tempfile = tempfile.NamedTemporaryFile(
                mode="r+b", suffix=".fits")
            self.hdulist[hdulist_index].writeto(self._tempfile.name)
        return self._tempfile.name

    def close(self):
        """
        Once we are done with the on disk content we should close the filehandle.
        """
        if self._tempfile is not None:
            self._tempfile.close()
            self._tempfile = None

    @property
    def comparison_image(self):
        if self.comparison_image_list[self.comparison_image_index]["REFERENCE"] is None:
            self.retrieve_comparison_image()
        row = self.comparison_image_list[self.comparison_image_list["ID"] == self.comparison_image_index]
        return row["REFERENCE"][0]

    @property
    def comparison_image_index(self):
        if self._comparison_image_index is None:
            ## Display a list of possible comparison images and ask user to select one.
            self.comparison_image_list.pprint()
            self.comparison_image_index = int(input("SELECT ROW NUMBER OF DESIRED COMPARISON IMAGE: "))
        return self._comparison_image_index

    @comparison_image_index.setter
    def comparison_image_index(self, comparison_image_index):
        self._comparison_image_index = comparison_image_index

    @property
    def comparison_image_list(self):
        """
        returns a list of possible comparison images for the current cutout.  Will query CADC to create the list when
        first called.
        @rtype: Table
        """

        if self._comparison_image_list is not None:
            return self._comparison_image_list

        ref_ra = self.reading.ra * units.degree
        ref_dec = self.reading.dec * units.degree
        radius = self.radius is not None and self.radius or config.read('CUTOUTS.SINGLETS.RADIUS') * units.arcminute
        print(("Querying CADC for list of possible comparison images at RA: {}, DEC: {}, raidus: {}".format(ref_ra,
                                                                                                           ref_dec,
                                                                                                           radius)))
        query_result = storage.cone_search(ref_ra, ref_dec, radius, radius)  # returns an astropy.table.table.Table
        print(("Got {} possible images".format(len(query_result))))
        ans = input("Do you want to lookup IQ? (y/n)")
        print("Building table for presentation and selection")
        if ans == "y":
            print("Including getting fwhm which is a bit slow.")
        comparison_image_list = []
        if len(query_result['collectionID']) > 0:  # are there any comparison images even available on that sky?
            index = -1
            for row in query_result:
                expnum = row['collectionID']
                if expnum in self._bad_comparison_images:
                    continue
                date = Time(row['mjdate'], format='mjd').mpc
                if Time(row['mjdate'], format='mjd') < Time('2013-01-01 00:00:00', format='iso'):
                    continue
                exptime = row['exptime']
                if float(exptime) < 250:
                    continue
                filter_name = row['filter']
                if 'U' in filter_name:
                    continue
                if filter_name.lower() in self.hdulist[-1].header['FILTER'].lower():
                    filter_name = "* {:8s}".format(filter_name)
                fwhm = -1.0
                if ans == 'y':
                    try:
                        fwhm = "{:5.2f}".format(float(storage.get_fwhm_tag(expnum, 22)))
                    except:
                        pass
                index += 1
                comparison_image_list.append([index, expnum, date, exptime, filter_name, fwhm, None])
        self._comparison_image_list = Table(data=numpy.array(comparison_image_list),
                                            names=["ID", "EXPNUM", "DATE-OBS", "EXPTIME", "FILTER", "FWHM", "REFERENCE"])
        return self._comparison_image_list


    def retrieve_comparison_image(self):
        """
        Search the DB for a comparison image for this cutout.
        """
        # selecting comparator when on a comparator should load a new one.
        collectionID = self.comparison_image_list[self.comparison_image_index]['EXPNUM']
        ref_ra = self.reading.ra * units.degree
        ref_dec = self.reading.dec * units.degree
        radius = self.radius is not None and self.radius or config.read('CUTOUTS.SINGLETS.RADIUS') * units.arcminute
        try:
            comparison = collectionID
            hdu_list = storage.ra_dec_cutout(storage.dbimages_uri(comparison),
                                             SkyCoord(ref_ra, ref_dec), radius)
            ccd = int(hdu_list[-1].header.get('EXTVER', 0))
            obs = Observation(str(comparison), 'p',
                              ccdnum=ccd)
            x = hdu_list[-1].header.get('NAXIS1', 0) // 2.0
            y = hdu_list[-1].header.get('NAXIS2', 0) // 2.0
            ref_ra = self.reading.ra * units.degree
            ref_dec = self.reading.dec * units.degree
            reading = SourceReading(x, y, self.reading.x, self.reading.y,
                                    ref_ra, ref_dec, self.reading.x, self.reading.y, obs)
            self.comparison_image_list[self.comparison_image_index]["REFERENCE"] = SourceCutout(reading, hdu_list)
        except Exception as ex:
            print(traceback.format_exc())
            print(ex)
            print("Failed to load comparison image;")
            self.comparison_image_index = None
            logger.error("{} {}".format(type(ex), str(ex)))
            logger.error(traceback.format_exc())





"""Create the mopheader file.

A mopheader is a FITS Primary HDU that contains the header quantities needed for an astrometric/photometric and time
measurement of a KBO in the image.  Basically the WCS transforms.
"""
from astropy import units
from astropy.coordinates import SkyCoord
import math

__author__ = 'jjk'

from astropy.time import Time, TimeDelta
from astropy.io import fits
import logging

from ossos import util
from ossos.wcs import WCS


class MOPHeader(fits.Header):

    def __init__(self, header):
        super(self.__class__, self).__init__()

        self.mop_keywords = {'NAXIS1': 'NAXIS1',  # Array X size, FITS
                             'NAXIS2': 'NAXIS2',  # Array Y size, FITS
                             'DETSEC': 'DETSEC',  # LOCATION of this ARRAY within the MEF
                             'CRPIX1': 'CRPIX1',
                             'CRPIX2': 'CRPIX2',
                             'CRVAL1': 'CRVAL1',
                             'CRVAL2': 'CRVAL2',
                             'RA_DEG': 'RA_DEG',
                             'DEC_DEG': 'DEC_DEG',
                             'EXPTIME': 'EXPTIME',
                             'EXPNUM': 'EXPNUM',
                             'RDNOIS': 'RDNOIS',
                             'GAIN': 'GAIN'}

        self._DET_X_CEN = 11604.5
        self._DET_Y_CEN = 9681
        try:
            self.wcs = WCS(header)
        except:
            self.wcs = None

        self.input_header = header

        for keyword in ['CRPIX1', 'CRPIX2', 'CRVAL1', 'CRVAL2', 'RDNOIS', 'GAIN', 'EXPTIME', 'PIXSCAL', 'EXTNO']:
            self[keyword] = self.__getattribute__(keyword.lower())


    def __getitem__(self, item):
        try:
            return self.__getattribute__(item.upper())
        except:
            raise KeyError(item)

    @property
    def crpix1(self):
            return self.crpix[0]

    @property
    def crpix2(self):
            return self.crpix[1]

    @property
    def crval1(self):
            return self.crval[0]

    @property
    def crval2(self):
            return self.crval[1]


    @property
    def crpix(self):
        """
        The location of the reference coordinate in the pixel frame.

        First simple respond with the header values, if they don't exist try usnig the DETSEC values
        @rtype: float, float
        """
        try:
            return self.wcs.crpix1, self.wcs.crpix2
        except Exception as ex:
            logging.debug("Couldn't get CRPIX from WCS: {}".format(ex))
            logging.debug("Switching to use DATASEC for CRPIX value computation.")

        try:
            (x1, x2), (y1, y2) = util.get_pixel_bounds_from_datasec_keyword(self[self.mop_keywords['DETSEC']])
            dx = float(self[self.mop_keywords['NAXIS1']])
            dy = float(self[self.mop_keywords['NAXIS2']])
        except KeyError as ke:
            raise KeyError("Header missing keyword: {}, required for CRPIX[12] computation".format(ke.args[0]))

        crpix1 = self._DET_X_CEN - (x1 + x2)/2. + dx/2.
        crpix2 = self._DET_Y_CEN - (y1 + y2)/2. + dy/2.

        return crpix1, crpix2

    @property
    def mjdobsc(self):
        """Given a CFHT Megaprime image header compute the center of exposure.

        This routine uses the calibration provide by Richard Wainscoat:

        From: Richard Wainscoat <rjw@ifa.hawaii.edu>
        Subject: Fwd: timing for MegaCam
        Date: April 24, 2015 at 7:23:09 PM EDT
        To: David Tholen <tholen@ifa.hawaii.edu>

        Looks like the midpoint of the exposure is now UTCEND - 0.73 sec - (exposure time  / 2  )
        Wobble on the 0.73 sec is +/- 0.15 sec.

        @rtype : float
        """
        #TODO Check if this exposure was taken afer or before correction needed.

        try:
            utc_end = self[self.mop_keywords.get('UTCEND', 'UTCEND')]
            exposure_time = float(self[self.mop_keywords.get('EXPTIME', 'EXPTIME')])
            date_obs = self[self.mop_keywords.get('DATE-OBS', 'DATE-OBS')]
        except KeyError as ke:
            raise KeyError("Header missing keyword: {}, required for MJD-OBSC computation".format(ke.args[0]))

        utc_end = Time(date_obs+"T"+utc_end)
        utc_cen = utc_end - TimeDelta(0.73, format='sec') - TimeDelta(exposure_time/2.0, format='sec')
        return utc_cen.mjd

    @property
    def crval(self):
        """
        Get the world coordinate of the reference pixel.

        @rtype: float, float
        """
        try:
            return self.wcs.crval1, self.wcs.crval2
        except Exception as ex:
            logging.debug("Couldn't get CRVAL from WCS: {}".format(ex))
            logging.debug("Trying RA/DEC values")

        try:
            return (float(self[self.mop_keywords.get('RA-DEG', 'RA-DEG')]),
                    float(self[self.mop_keywords.get('DEC-DEG', 'DEC-DEG')]))
        except KeyError as ke:
            KeyError("Can't build CRVAL1/2 missing keyword: {}".format(ke.args[0]))

    @property
    def pixscal(self):
        """Return the pixel scale of the detector, in arcseconds.

        This routine first attempts to compute the size of a pixel using the WCS.  Then looks for PIXSCAL in header.

        @return: pixscal
        @rtype: float
        """
        try:
            (x, y) = self.crval
            p1 = SkyCoord(self.wcs.xy2sky(x, y) * units.degree)
            p2 = SkyCoord(self.wcs.xy2sky(x+1, y+1) * units.degree)

            return p1.separation(p2).to(units.arcsecond).value/math.sqrt(2)
        except Exception as ex:
            logging.debug("Failed to compute PIXSCALE using WCS: {}".format(ex))

        return float(self[self.mop_keywords.get('PIXSCAL', 'PIXSCAL')])

    @property
    def extno(self):
        return int(self[self.mop_keywords.get('CHIPID', 'CHIPID')]) + 1


    @property
    def gain(self):
        """
        @rtype: float
        @return: The CCD gain
        """
        return float(self[self.mop_keywords.get('GAIN', 'GAIN')])

    @property
    def rdnoise(self):
        """
        @rtype: float
        @return: The CCD ReadNoise
        """
        return float(self[self.mop_keywords.get('RDNOISE', 'RDNOISE')])

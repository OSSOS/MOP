"""Create the mopheader file.

A mopheader is a FITS Primary HDU that contains the header quantities needed for an astrometric/photometric and time
measurement of a KBO in the image.  Basically the WCS transforms.
"""
__author__ = 'jjk'

import logging
import math

from astropy import units
from astropy.coordinates import SkyCoord
from astropy.time import Time, TimeDelta
from astropy.io import fits

from ossos import util
from ossos.wcs import WCS


class MOPHeader(fits.Header):
    """
    MOPHeader creates a fits.Header object that is derived from an input image (only works for MegaPrime right now)
    but with some keyword values modified to provide the correct values to down-stream processing.

    Currently for MegaPrime the only major use of mopheader is to correct the FITS header expsoure time.
    """

    def __init__(self, header):
        super(self.__class__, self).__init__(header)

        self.mop_keywords = ['NAXIS1',
                             'NAXIS2',
                             'CRPIX1',
                             'CRPIX2',
                             'CRVAL1',
                             'CRVAL2',
                             'PIXSCALE',
                             'EXPTIME',
                             'EXPNUM',
                             'RDNOISE',
                             'PHPADU',
                             'CHIPNUM',
                             'DETECTOR',
                             'MJD-OBSC']
        self.mop_comments = {'MJD-OBSC': "MJD at observation center time"}

        self._DET_X_CEN = 11604.5
        self._DET_Y_CEN = 9681
        try:
            self.wcs = WCS(header)
        except:
            self.wcs = None

        for keyword in self.mop_keywords:
            try:
                comment = self.comments[keyword]
            except:
                comment = self.mop_comments.get(keyword, None)
            try:
  
                self[keyword] = (self.__getattribute__(keyword.lower().replace("-", "_")), comment)
            except Exception as ex:
                logging.debug("Failed to build mopkeyword: {} -> {} using default".format(keyword, ex))
                pass

        for keyword in list(self.keys()):
            if keyword not in self.mop_keywords:
                try:
                   self.__delitem__(keyword)
                except:
                   pass
        self['MOP_VER'] = 1.21

    def writeto(self, filename, **kwargs):
        """
        Write the header to a fits file.
        :param filename:
        :return:
        """
        fits.PrimaryHDU(header=self).writeto(filename, output_verify='ignore', **kwargs)

    @property
    def crpix1(self):
        return round(self.crpix[0], 2)

    @property
    def crpix2(self):
        return round(self.crpix[1], 2)

    @property
    def crval1(self):
        return round(self.crval[0], 5)

    @property
    def crval2(self):
        return round(self.crval[1], 5)

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
            (x1, x2), (y1, y2) = util.get_pixel_bounds_from_datasec_keyword(self['DETSEC'])
            dx = float(self['NAXIS1'])
            dy = float(self['NAXIS2'])
        except KeyError as ke:
            raise KeyError("Header missing keyword: {}, required for CRPIX[12] computation".format(ke.args[0]))

        crpix1 = self._DET_X_CEN - (x1 + x2) / 2. + dx / 2.
        crpix2 = self._DET_Y_CEN - (y1 + y2) / 2. + dy / 2.

        return crpix1, crpix2

    @property
    def mjd_obsc(self):
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
        # TODO Check if this exposure was taken afer or before correction needed.

        try:
            utc_end = self['UTCEND']
            exposure_time = float(self['EXPTIME'])
            date_obs = self['DATE-OBS']
        except KeyError as ke:
            raise KeyError("Header missing keyword: {}, required for MJD-OBSC computation".format(ke.args[0]))

        utc_end = Time(date_obs + "T" + utc_end)
        utc_cen = utc_end - TimeDelta(0.73, format='sec') - TimeDelta(exposure_time / 2.0, format='sec')
        return round(utc_cen.mjd, 7)

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
            return (float(self['RA-DEG']),
                    float(self['DEC-DEG']))
        except KeyError as ke:
            KeyError("Can't build CRVAL1/2 missing keyword: {}".format(ke.args[0]))

    @property
    def pixscale(self):
        """Return the pixel scale of the detector, in arcseconds.

        This routine first attempts to compute the size of a pixel using the WCS.  Then looks for PIXSCAL in header.

        @return: pixscal
        @rtype: float
        """
        try:
            (x, y) = self['NAXIS1'] / 2.0, self['NAXIS2'] / 2.0
            p1 = SkyCoord(*self.wcs.xy2sky(x, y) * units.degree)
            p2 = SkyCoord(*self.wcs.xy2sky(x + 1, y + 1) * units.degree)
            return round(p1.separation(p2).to(units.arcsecond).value / math.sqrt(2), 3)
        except Exception as ex:
            logging.debug("Failed to compute PIXSCALE using WCS: {}".format(ex))

        return float(self['PIXSCAL'])

    @property
    def chipnum(self):
        return int(self['EXTVER']) + 1

    @property
    def phpadu(self):
        """
        @rtype: float
        @return: The CCD gain
        """
        return round(float(self['GAIN']), 2)

    @property
    def detector(self):
        """
        @rtype: basestring
        :return: The DETECTOR keyword value
        """
        return self.get('DETECTOR', self['INSTRUME'])


def main(filename):
    mop_header = MOPHeader(fits.open(filename)[0].header)
    output_filename = filename.split('.')[0] + ".mopheader"
    mop_header.writeto(output_filename, clobber=True)
    return output_filename

if __name__ == '__main__':
    import sys

    main(sys.argv[1])

from unittest import TestCase
from astropy.io import fits

__author__ = 'jjk'

from ossos import mopheader


class TestMOPHeader(TestCase):
    """
    Test that the MOPHeader object created in Python matches the one that was created by FORTRAN stepZjmp
    """

    def setUp(self):
        self.filename = "../data/1616681p22.fits"
        self.old_mop_header = fits.open('../data/1616681p22.mopheader_v1.20')[0].header
        self.mop_header = mopheader.MOPHeader(fits.open(self.filename)[0].header)

    def test_equal_value_keywords(self):

        for key in ['CHIPNUM', 'EXPNUM', 'NAXIS1', 'NAXIS2', 'DETECTOR']:
            self.assertEqual(self.mop_header[key], self.old_mop_header[key])

    def test_real_value_keywords(self):

        key_precision_map = {
            'CRVAL1': 5,
            'CRVAL2': 5,
            'CRPIX1': 2,
            'CRPIX2': 2,
            'PIXSCALE': 2,
            'EXPTIME': 2,
            'PHPADU': 2,
            'RDNOISE': 2
        }
        for keyword in key_precision_map:
            self.assertAlmostEqual(self.mop_header['CRVAL1'],
                                   self.old_mop_header['CRVAL1'],
                                   places=key_precision_map[keyword])

    def test_date_difference(self):
        """
        Compare the times, the old mopheaders had a different time computation.
        :return:
        """
        self.assertAlmostEqual(self.mop_header['MJD-OBSC']-self.old_mop_header['MJD-OBSC'], 46/3600.0/24.0, places=5)

    def test_no_wcs(self):
        """
        Check that the correct header is built when the WCS can't be.
        :return:
        """
        header = fits.open(self.filename)[0].header
        del header['CR*']
        mop_header = mopheader.MOPHeader(header)
        self.assertIsInstance(mop_header, mopheader.MOPHeader)
        for key in ['PIXSCALE', 'EXPNUM', 'CHIPNUM']:
            self.assertAlmostEqual(mop_header[key], self.mop_header[key], places=3)

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from astropy.io import fits
from hamcrest import assert_that, equal_to
from mock import patch, ANY

from tests.base_tests import FileReadingTestCase
from ossos.downloads.cutouts.calculator import CoordinateConverter
from ossos.downloads.cutouts.source import SourceCutout
from ossos.downloads.core import ApcorData
from ossos.astrom import AstromParser


class SourceSnapshotIntegrationTest(FileReadingTestCase):
    def path(self, relative):
        return self.get_abs_path("data/image_reading/%s" % relative)

    def setUp(self):
        test_file = self.path("realstest2.measure3.reals.astrom")
        astrom_data = AstromParser().parse(test_file)

        self.reading = astrom_data.get_sources()[0].get_reading(0)

        # Load the real image
        hdulist = fits.open(self.path("cutout-1616687p.fits"))

        with open(self.path("1616687p10.apcor")) as fh:
            apcor = ApcorData.from_string(fh.read())

        # NOTE: the test image
        # vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616687/1616687p.fits
        # has cutout: [11][1449:1199,1429:1179] and is inverted.
        #
        # Therefore the first reading at (772.13, 3334.70) in observed
        # coordinates is at
        # (NAX1 - max cutout x, NAX2 - max cutout y)
        #  = (2112 - 1449, 4644 - 1429) = (663, 3215)
        self.original_pixel_x = 663
        self.original_pixel_y = 3215

        self.original_observed_x = 772.13
        self.original_observed_y = 3334.70

        x_offset = self.original_observed_x - self.original_pixel_x
        y_offset = self.original_observed_y - self.original_pixel_y

        self.undertest = SourceCutout(self.reading, hdulist, apcor=apcor)

        assert_that(self.undertest.observed_x, equal_to(self.original_observed_x))
        assert_that(self.undertest.observed_y, equal_to(self.original_observed_y))

        assert_that(self.undertest.pixel_x, equal_to(self.original_pixel_x))
        assert_that(self.undertest.pixel_y, equal_to(self.original_pixel_y))

    @patch("ossos.wcs.xy2sky")
    def test_update_ra_dec(self, mock_xy2sky):
        new_ra = 2000.0
        new_dec = -11.0
        mock_xy2sky.return_value = (new_ra, new_dec)

        diff_x = 5.5
        diff_y = 4.5
        new_pixel_x = self.original_pixel_x + diff_x
        new_pixel_y = self.original_pixel_y + diff_y

        self.undertest.update_pixel_location((new_pixel_x, new_pixel_y))

        assert_that(self.undertest.ra, equal_to(new_ra))
        assert_that(self.undertest.dec, equal_to(new_dec))

        # Check it was called with the correct values:
        #   x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord

        # These values from FITS header:

        crpix1 = 6.779623933420000E+03
        crpix2 = -3.343209710100000E+03

        crval1 = 211.828420000
        crval2 = -11.8333100000

        # CD1_1, CD1_2, CD2_1, CD2_2
        cd = [[-5.156837193510000E-05, -3.588985558218000E-07],
              [-1.674871346376000E-07, 5.178515755263000E-05]]

        # PV1_0 - PV1_10, PV2_0 - PV2_10
        pv = [[-1.909806626877E-03, 1.00815723310, 1.983393486250E-03,
               0.00000000000, 4.623172227977E-05, -1.020423075276E-05,
               2.258600565396E-05, -2.354656733463E-02, -1.671912720931E-04,
               -2.339051960031E-02, -2.903290518437E-05],
              [-7.294883106769E-04, 1.00392029973, 1.983331686156E-03,
               0.00000000000, 3.022667872225E-05, 4.509364811534E-05,
               -5.385195875663E-06, -2.333221106141E-02, -1.789051786060E-04,
               -2.348803123711E-02, -2.465552723903E-05]]

        # NORDFIT
        nord = 3

        # NOTE: the x and y passed in must be the updated OBSERVED coordinates
        mock_xy2sky.assert_called_once_with(
            new_pixel_x, new_pixel_y,
            crpix1, crpix2, crval1, crval2,
            cd, pv, nord)

    @patch("ossos.daophot.phot_mag")
    def test_get_observed_magnitude(self, mock_phot_mag):
        self.undertest.get_observed_magnitude()

        # NOTE: the x and y passed in must be the PIXEL coordinates
        mock_phot_mag.assert_called_once_with(
            ANY,
            self.original_pixel_x,
            self.original_pixel_y,
            aperture=5.0,
            sky=21.0,
            swidth=5.0,
            apcor=0.23,
            maxcount=30000.0)


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import patch, Mock

from tests.base_tests import FileReadingTestCase
from ossos.cutouts import CoordinateConverter
from ossos.astrom import AstromParser
from ossos.gui.image import DownloadedFitsImage
from ossos.gui.models import ImageReading


class ImageReadingIntegrationTest(FileReadingTestCase):
    def path(self, relative):
        return self.get_abs_path("data/image_reading/%s" % relative)

    def setUp(self):
        test_file = self.path("realstest2.measure3.reals.astrom")
        astrom_data = AstromParser().parse(test_file)

        self.reading = astrom_data.get_sources()[0].get_reading(0)

        # Load the real image
        with open(self.path("cutout-1616687p.fits")) as fh:
            fits_str = fh.read()

        with open(self.path("1616687p10.apcor")) as fh:
            apcor_str = fh.read()

        self.image = DownloadedFitsImage(fits_str,
                                         Mock(spec=CoordinateConverter),
                                         apcor_str=apcor_str)

        self.undertest = ImageReading(self.reading, self.image)

    @patch("ossos.wcs.xy2sky")
    def test_update_ra_dec(self, mock_xy2sky):
        new_ra = 2000.0
        new_dec = -11.0
        mock_xy2sky.return_value = (new_ra, new_dec)

        new_x = 773.50
        new_y = 3333.75

        self.undertest.update_x(new_x)
        self.undertest.update_y(new_y)

        assert_that(self.undertest.ra, equal_to(new_ra))
        assert_that(self.undertest.dec, equal_to(new_dec))

        # Check it was called with the correct values from the Astrom file
        # and FITS header:
        #   x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord

        # These values from FITS header:

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

        # The rest are from the Astrom header
        mock_xy2sky.assert_called_once_with(new_x, new_y, 7442.65, -128.69,
                                            211.82842, -11.83331, cd, pv, nord)

    @patch("ossos.daophot.phot_mag")
    def test_get_observed_magnitude(self, mock_phot_mag):
        self.undertest.get_observed_magnitude()

        mock_phot_mag.assert_called_once_with(
            self.image.as_file().name,
            772.13, 3334.70,
            aperture=5.0,
            sky=21.0,
            swidth=5.0,
            apcor=0.23,
            maxcount=30000.0)


if __name__ == '__main__':
    unittest.main()

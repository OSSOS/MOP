__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os

from mock import Mock, patch
from hamcrest import assert_that, not_none, none, equal_to

from test.base_tests import FileReadingTestCase
from pymop.gui.image import DownloadedFitsImage, ApcorData


class DownloadedFitsImageTest(FileReadingTestCase):
    def setUp(self):
        with open(self.get_abs_path("data/testimg.fits"), "rb") as fh:
            self.strdata = fh.read()

        self.apcor_str = "4 15   0.19   0.01"
        self.coord_converter = Mock()

    def test_create_in_memory_image(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=True)

        # Breaking interface for testing purposes
        assert_that(fitsimage._hdulist, not_none())
        assert_that(fitsimage._tempfile, none())

    def test_create_on_disk_image(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=False)

        # Breaking interface for testing purposes
        assert_that(fitsimage._hdulist, none())
        assert_that(fitsimage._tempfile, not_none())

    def test_in_memory_as_file(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=True)
        assert_that(os.path.exists(fitsimage.as_file().name))

    def test_on_disk_as_hdulist(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=False)

        assert_that(fitsimage._hdulist, none())
        assert_that(fitsimage.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))
        assert_that(fitsimage._hdulist, not_none())

    def test_in_memory_as_hdulist(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=True)
        assert_that(fitsimage.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_on_disk_as_file(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=False)
        assert_that(os.path.exists(fitsimage.as_file().name))

    def test_close(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=True)
        as_file = fitsimage.as_file()

        fitsimage.close()

        assert_that(not os.path.exists(as_file.name))

    @patch("pymop.tools.daophot.phot_mag")
    def test_get_observed_magnitude(self, mock_phot_mag):
        fitsimage = DownloadedFitsImage(self.strdata, self.apcor_str, self.coord_converter,
                                        in_memory=True)
        x = 1500
        y = 2500
        fitsimage.get_observed_magnitude(x, y)

        mock_phot_mag.assert_called_once_with(
            fitsimage.as_file().name, x, y, aperture=4.0, sky=16.0, swidth=4.0,
            apcor=0.19, maxcount=30000.0
        )


class ApcorDataTest(unittest.TestCase):
    def setUp(self):
        """
        Example data from
        vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd22/1616681p22.apcor
        """
        self.ap_in = 4
        self.ap_out = 15
        self.apcor = 0.19
        self.apcor_err = 0.01

        self.undertest = ApcorData(self.ap_in, self.ap_out, self.apcor,
                                   self.apcor_err)

    def test_basic_properties(self):
        assert_that(self.undertest.ap_in, equal_to(self.ap_in))
        assert_that(self.undertest.ap_out, equal_to(self.ap_out))
        assert_that(self.undertest.apcor, equal_to(self.apcor))
        assert_that(self.undertest.apcor_err, equal_to(self.apcor_err))

    def test_generated_properties(self):
        assert_that(self.undertest.aperture, equal_to(self.ap_in))
        assert_that(self.undertest.sky, equal_to(self.ap_out + 1))
        # TODO verify this with JJ, seems suspicious
        assert_that(self.undertest.swidth, equal_to(self.ap_in))

    def test_from_raw_string(self):
        rawstr = "4 15   0.19   0.01\n"

        # NOTE: not undertest
        self.parsed = ApcorData.from_raw_string(rawstr)

        assert_that(self.parsed.ap_in, equal_to(self.ap_in))
        assert_that(self.parsed.ap_out, equal_to(self.ap_out))
        assert_that(self.parsed.apcor, equal_to(self.apcor))
        assert_that(self.parsed.apcor_err, equal_to(self.apcor_err))


if __name__ == '__main__':
    unittest.main()

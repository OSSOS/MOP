__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os

from mock import Mock
from hamcrest import assert_that, not_none, none, equal_to

from tests.base_tests import FileReadingTestCase
from ossos.download.data import DownloadedFitsImage, ApcorData


class DownloadedFitsImageTest(FileReadingTestCase):
    def setUp(self):
        with open(self.get_abs_path("data/testimg.fits"), "rb") as fh:
            self.strdata = fh.read()

        self.apcor_str = "4 15   0.19   0.01"
        self.coord_converter = Mock()

    def test_as_file(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.coord_converter,
                                        self.apcor_str)
        assert_that(os.path.exists(fitsimage.as_file().name))

    def test_close(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.coord_converter,
                                        self.apcor_str)
        as_file = fitsimage.as_file()
        assert_that(os.path.exists(as_file.name), equal_to(True))

        fitsimage.close()
        assert_that(os.path.exists(as_file.name), equal_to(False))

    def test_no_apcord_data(self):
        fitsimage = DownloadedFitsImage(self.strdata, self.coord_converter)
        assert_that(fitsimage.has_apcord_data(), equal_to(False))
        assert_that(fitsimage.get_apcor_data(), none())


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

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from astropy.io import fits
from hamcrest import assert_that, equal_to
from mock import patch

from tests.base_tests import FileReadingTestCase
from ossos import naming, storage
from ossos.astrom import AstromParser
from ossos.naming import ProvisionalNameGenerator


class ProvisionalNameGeneratorTest(FileReadingTestCase):
    def setUp(self):
        self.undertest = ProvisionalNameGenerator()

    def parse_astrom_header(self, filename="data/naming/E+3+0_21.measure3.cands.astrom"):
        return AstromParser().parse(self.get_abs_path(filename)).get_sources()[0].get_reading(
            0).get_observation_header()

    def parse_fits_header(self, filename="data/naming/cutout-1616690p.fits"):
        return fits.open(self.get_abs_path(filename))[0].header

    @patch("ossos.storage.increment_object_counter")
    @patch("ossos.storage.get_object_counter")
    def test_generate_name(self, get_object_counter, increment_object_counter):
        get_object_counter.return_value = "00"

        astrom_header = self.parse_astrom_header()
        fits_header = self.parse_fits_header()

        assert_that(self.undertest.generate_name(astrom_header, fits_header),
                    equal_to("O13AE01"))

        get_object_counter.assert_called_once_with(storage.MEASURE3, "O13AE")
        increment_object_counter.assert_called_once_with(storage.MEASURE3, "O13AE")

    @patch("ossos.storage.increment_object_counter")
    @patch("ossos.storage.get_object_counter")
    def test_generate_name_object_header_has_epoch(self, get_object_counter, _):
        get_object_counter.return_value = "00"

        astrom_header = self.parse_astrom_header()
        fits_header = self.parse_fits_header()
        fits_header["OBJECT"] = "13A" + fits_header["OBJECT"]

        assert_that(self.undertest.generate_name(astrom_header, fits_header),
                    equal_to("O13AE01"))


class EncodingTest(unittest.TestCase):
    def test_base26_boundary(self):
        assert_that(naming.base26encode(0), equal_to("A"))
        assert_that(naming.base26encode(1), equal_to("B"))
        assert_that(naming.base26encode(9999999), equal_to("VWYXJ"))

    def test_base26_realistic_exposure_numbers(self):
        assert_that(naming.base26encode(1616703), equal_to("DNZOX"))
        assert_that(naming.base26encode(1616704), equal_to("DNZOY"))
        assert_that(naming.base26encode(1616705), equal_to("DNZOZ"))

    def test_base36_encode_decode_1(self):
        assert_that(naming.base36encode(1), equal_to("1"))
        assert_that(naming.base36decode("1"), equal_to(1))

    def test_base36_encode_10(self):
        assert_that(naming.base36encode(10), equal_to("A"))
        assert_that(naming.base36decode("A"), equal_to(10))

    def test_base36_encode_100(self):
        assert_that(naming.base36encode(100), equal_to("2S"))
        assert_that(naming.base36decode("2S"), equal_to(100))

    def test_base36_encode_10000(self):
        assert_that(naming.base36encode(10000), equal_to("7PS"))
        assert_that(naming.base36decode("7PS"), equal_to(10000))

    def test_base36_encode_pad_short(self):
        assert_that(naming.base36encode(1, pad_length=2), equal_to("01"))

    def test_base36_encode_pad_long(self):
        assert_that(naming.base36encode(10000, pad_length=2), equal_to("7PS"))


if __name__ == '__main__':
    unittest.main()

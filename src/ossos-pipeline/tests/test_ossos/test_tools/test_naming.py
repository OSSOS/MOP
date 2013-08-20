__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from astropy.io import fits
from hamcrest import assert_that, equal_to
from mock import patch

from tests.base_tests import FileReadingTestCase
from ossos import storage
from ossos.astrom import AstromParser
from ossos.naming import ProvisionalNameGenerator, DryRunNameGenerator


class ProvisionalNameGeneratorTest(FileReadingTestCase):
    def setUp(self):
        self.undertest = ProvisionalNameGenerator()

    def parse_astrom_header(self, filename="data/naming/E+3+0_21.measure3.cands.astrom"):
        return AstromParser().parse(self.get_abs_path(filename)).get_sources()[0].get_reading(
            0).get_observation_header()

    def parse_fits_header(self, filename="data/naming/cutout-1616690p.fits"):
        return fits.open(self.get_abs_path(filename))[0].header

    @patch("ossos.storage.increment_object_counter")
    def test_generate_name(self, increment_object_counter):
        increment_object_counter.return_value = "01"

        astrom_header = self.parse_astrom_header()
        fits_header = self.parse_fits_header()

        assert_that(self.undertest.generate_name(astrom_header, fits_header),
                    equal_to("O13AE01"))

        increment_object_counter.assert_called_once_with(storage.MEASURE3, "O13AE")

    @patch("ossos.storage.increment_object_counter")
    def test_generate_name_object_header_has_epoch(self, increment_object_counter):
        increment_object_counter.return_value = "01"

        astrom_header = self.parse_astrom_header()
        fits_header = self.parse_fits_header()
        fits_header["OBJECT"] = "13A" + fits_header["OBJECT"]

        assert_that(self.undertest.generate_name(astrom_header, fits_header),
                    equal_to("O13AE01"))

    @patch("ossos.storage.increment_object_counter")
    def test_dry_run_generate_name(self, increment_object_counter):
        self.undertest = DryRunNameGenerator()

        increment_object_counter.return_value = "01"

        astrom_header = self.parse_astrom_header()
        fits_header = self.parse_fits_header()

        assert_that(self.undertest.generate_name(astrom_header, fits_header),
                    equal_to("DRY0001"))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import Mock

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.gui.image import DownloadedFitsImage
from ossos.gui.models import ImageReading


class ImageReadingTest(FileReadingTestCase):
    def setUp(self):
        test_file = "data/image_reading/realstest2.measure3.reals.astrom"
        astrom_data = AstromParser().parse(self.get_abs_path(test_file))

        self.reading = astrom_data.get_sources()[0].get_reading(0)
        self.original_observed_x = self.reading.x
        self.original_observed_y = self.reading.y

        self.image = Mock(spec=DownloadedFitsImage)

        self.offset_x = 10
        self.offset_y = 15

        def convert_to_pixel(point):
            return point[0] - self.offset_x, point[1] - self.offset_y

        def convert_to_observed(point):
            return point[0] + self.offset_x, point[1] + self.offset_y

        self.image.get_pixel_coordinates = convert_to_pixel
        self.image.get_observed_coordinates = convert_to_observed

        self.undertest = ImageReading(self.reading, self.image)

        self.mock_update_ra_dec = Mock()
        self.undertest._update_ra_dec = self.mock_update_ra_dec

    def test_xy_coordinate_systems(self):
        assert_that(self.undertest.observed_x,
                    equal_to(self.original_observed_x))
        assert_that(self.undertest.observed_y,
                    equal_to(self.original_observed_y))

        expected_pixel_x = self.original_observed_x - self.offset_x
        expected_pixel_y = self.original_observed_y - self.offset_y

        assert_that(self.undertest.pixel_x, equal_to(expected_pixel_x))
        assert_that(self.undertest.pixel_y, equal_to(expected_pixel_y))

        assert_that(self.undertest.observed_source_point,
                    equal_to((self.original_observed_x, self.original_observed_y)))
        assert_that(self.undertest.pixel_source_point,
                    equal_to((expected_pixel_x, expected_pixel_y)))

    def test_xy_corrected(self):
        assert_that(self.undertest.observed_x,
                    equal_to(self.original_observed_x))
        assert_that(self.undertest.observed_y,
                    equal_to(self.original_observed_y))

        assert_that(self.undertest.is_corrected(), equal_to(False))

        diff_x = 2.5
        diff_y = 3.5

        new_pixel_x = self.undertest.pixel_x + diff_x
        new_pixel_y = self.undertest.pixel_y + diff_y

        self.undertest.update_pixel_location((new_pixel_x, new_pixel_y))

        assert_that(self.undertest.pixel_x, equal_to(new_pixel_x))
        assert_that(self.undertest.pixel_y, equal_to(new_pixel_y))

        assert_that(self.undertest.observed_x,
                    equal_to(self.original_observed_x + diff_x))
        assert_that(self.undertest.observed_y,
                    equal_to(self.original_observed_y + diff_y))

        assert_that(self.undertest.is_corrected(), equal_to(True))

    def test_update_xy_updates_ra_dec(self):
        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_pixel_location((665.0, 3215.0))

        _ = self.undertest.dec

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

    def test_ra_dec_updates_lazilly(self):
        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_pixel_location((665.0, 3215.0))

        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_pixel_location((665.0, 3218.0))

        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        _ = self.undertest.ra

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        _ = self.undertest.dec

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        self.undertest.update_pixel_location((667.2, 3216.6))

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        _ = self.undertest.dec
        _ = self.undertest.ra

        assert_that(self.mock_update_ra_dec.call_count, equal_to(2))

    def test_original_xy(self):
        assert_that(self.undertest.observed_x, equal_to(self.original_observed_x))
        assert_that(self.undertest.observed_y, equal_to(self.original_observed_y))

        assert_that(self.undertest.original_observed_x,
                    equal_to(self.original_observed_x))
        assert_that(self.undertest.original_observed_y,
                    equal_to(self.original_observed_y))

        diff_x = 2.5
        diff_y = 3.5

        new_pixel_x = self.undertest.pixel_x + diff_x
        new_pixel_y = self.undertest.pixel_y + diff_y

        self.undertest.update_pixel_location((new_pixel_x, new_pixel_y))

        assert_that(self.undertest.observed_x,
                    equal_to(self.original_observed_x + diff_x))
        assert_that(self.undertest.observed_y,
                    equal_to(self.original_observed_y + diff_y))

        assert_that(self.undertest.original_observed_x,
                    equal_to(self.original_observed_x))
        assert_that(self.undertest.original_observed_y,
                    equal_to(self.original_observed_y))


if __name__ == '__main__':
    unittest.main()

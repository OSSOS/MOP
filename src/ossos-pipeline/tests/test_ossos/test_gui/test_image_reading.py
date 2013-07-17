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
        self.original_x = self.reading.x
        self.original_y = self.reading.y

        self.image = Mock(spec=DownloadedFitsImage)
        self.undertest = ImageReading(self.reading, self.image)

        self.mock_update_ra_dec = Mock()
        self.undertest._update_ra_dec = self.mock_update_ra_dec

    def test_xy_corrected(self):
        assert_that(self.undertest.x, equal_to(self.original_x))
        assert_that(self.undertest.y, equal_to(self.original_y))
        assert_that(self.undertest.is_corrected(), equal_to(False))

        new_x = 775.0
        new_y = 3335.2

        self.undertest.update_x(new_x)
        self.undertest.update_y(new_y)

        assert_that(self.undertest.x, equal_to(new_x))
        assert_that(self.undertest.y, equal_to(new_y))
        assert_that(self.undertest.is_corrected(), equal_to(True))

    def test_update_xy_updates_ra_dec(self):
        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_x(775.0)

        _ = self.undertest.dec

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

    def test_ra_dec_updates_lazilly(self):
        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_x(775.0)

        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        self.undertest.update_y(3335.2)

        assert_that(self.mock_update_ra_dec.call_count, equal_to(0))

        _ = self.undertest.ra

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        _ = self.undertest.dec

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        self.undertest.update_x(776.5)
        self.undertest.update_y(3336.7)

        assert_that(self.mock_update_ra_dec.call_count, equal_to(1))

        _ = self.undertest.dec
        _ = self.undertest.ra

        assert_that(self.mock_update_ra_dec.call_count, equal_to(2))

    def test_original_xy_ra_dec(self):
        assert_that(self.undertest.x, equal_to(self.original_x))
        assert_that(self.undertest.y, equal_to(self.original_y))

        assert_that(self.undertest.original_x, equal_to(self.original_x))
        assert_that(self.undertest.original_y, equal_to(self.original_y))

        new_x = 775.2
        new_y = 3335.2

        self.undertest.update_x(new_x)
        self.undertest.update_y(new_y)

        assert_that(self.undertest.x, equal_to(new_x))
        assert_that(self.undertest.y, equal_to(new_y))

        assert_that(self.undertest.original_x, equal_to(self.original_x))
        assert_that(self.undertest.original_y, equal_to(self.original_y))


if __name__ == '__main__':
    unittest.main()

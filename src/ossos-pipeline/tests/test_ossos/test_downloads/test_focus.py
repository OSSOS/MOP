__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, close_to, has_length, equal_to

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.download.focus import FocalPointCalculator


class FocalPointCalculatorTest(FileReadingTestCase):
    def setUp(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))
        self.source = astrom_data.get_sources()[0]
        self.reading0 = self.source.get_reading(0)
        self.reading1 = self.source.get_reading(1)
        self.reading2 = self.source.get_reading(2)

        self.undertest = FocalPointCalculator()

    def assert_tuples_almost_equal(self, actual, expected, delta=0.0000001):
        assert_that(actual[0], close_to(expected[0], delta))
        assert_that(actual[1], close_to(expected[1], delta))

    def test_get_focal_point_first_reading(self):
        self.assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading0, self.source),
            (583.42, 408.46))

    def test_get_focal_point_second_reading(self):
        self.assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading1, self.source),
            (586.18, 408.63))

    def test_get_focal_point_third_reading(self):
        self.assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading2, self.source),
            (587.80, 407.98))

    def test_calculate_focal_points(self):
        focal_points = self.undertest.calculate_focal_points(self.source)
        assert_that(focal_points, has_length(3))

        focal_point_0 = focal_points[0]
        assert_that(focal_point_0.reading, equal_to(self.reading0))
        self.assert_tuples_almost_equal(
            focal_point_0.point, (583.42, 408.46))

        focal_point_1 = focal_points[1]
        assert_that(focal_point_1.reading, equal_to(self.reading1))
        self.assert_tuples_almost_equal(
            focal_point_1.point, (586.18, 408.63))

        focal_point_2 = focal_points[2]
        assert_that(focal_point_2.reading, equal_to(self.reading2))
        self.assert_tuples_almost_equal(
            focal_point_2.point, (587.80, 407.98))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, close_to, has_length, equal_to

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.downloads.cutouts.focus import SingletFocalPointCalculator, TripletFocalPointCalculator


def assert_tuples_almost_equal(actual, expected, delta=0.0000001):
    assert_that(actual[0], close_to(expected[0], delta))
    assert_that(actual[1], close_to(expected[1], delta))


class SingletFocalPointCalculatorTest(FileReadingTestCase):
    def setUp(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))
        self.source = astrom_data.get_sources()[0]
        self.reading0 = self.source.get_reading(0)
        self.reading1 = self.source.get_reading(1)
        self.reading2 = self.source.get_reading(2)

        self.undertest = SingletFocalPointCalculator(self.source)

    def test_get_focal_point_first_reading(self):
        assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading0),
            (583.42, 408.46))

    def test_get_focal_point_second_reading(self):
        assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading1),
            (586.18, 408.63))

    def test_get_focal_point_third_reading(self):
        assert_tuples_almost_equal(
            self.undertest.calculate_focal_point(self.reading2),
            (587.80, 407.98))


class TripletFocalPointCalculatorTest(FileReadingTestCase):
    def setUp(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))
        self.source = astrom_data.get_sources()[0]
        self.reading0 = self.source.get_reading(0)
        self.reading1 = self.source.get_reading(1)
        self.reading2 = self.source.get_reading(2)

        self.undertest = TripletFocalPointCalculator(self.source)

    def test_calculate_focal_points(self):
        focal_points = self.undertest.calculate_focal_points(self.source)

        assert_that(focal_points, has_length(9))

        assert_that(focal_points[0].reading, equal_to(self.reading0))
        assert_tuples_almost_equal(focal_points[0].point, (560.06, 406.51))

        assert_that(focal_points[1].reading, equal_to(self.reading1))
        assert_tuples_almost_equal(focal_points[1].point, (583.42, 408.46))

        assert_that(focal_points[2].reading, equal_to(self.reading2))
        assert_tuples_almost_equal(focal_points[2].point, (608.48, 407.17))

        assert_that(focal_points[3].reading, equal_to(self.reading0))
        assert_tuples_almost_equal(focal_points[3].point, (562.82, 406.68))

        assert_that(focal_points[4].reading, equal_to(self.reading1))
        assert_tuples_almost_equal(focal_points[4].point, (586.18, 408.63))

        assert_that(focal_points[5].reading, equal_to(self.reading2))
        assert_tuples_almost_equal(focal_points[5].point, (611.24, 407.34))

        assert_that(focal_points[6].reading, equal_to(self.reading0))
        assert_tuples_almost_equal(focal_points[6].point, (564.44, 406.03))

        assert_that(focal_points[7].reading, equal_to(self.reading1))
        assert_tuples_almost_equal(focal_points[7].point, (587.80, 407.98))

        assert_that(focal_points[8].reading, equal_to(self.reading2))
        assert_tuples_almost_equal(focal_points[8].point, (612.86, 406.69))


if __name__ == '__main__':
    unittest.main()

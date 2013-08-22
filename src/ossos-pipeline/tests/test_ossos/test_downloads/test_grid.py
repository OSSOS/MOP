__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, call
from hamcrest import assert_that, equal_to, contains

from ossos import astrom
from ossos.downloads.cutouts.grid import CutoutGrid
from ossos.downloads.cutouts.source import SourceCutout


class CutoutGridTest(unittest.TestCase):
    def setUp(self):
        self.source = Mock(spec=astrom.Source)
        self.source.num_readings.return_value = 2

        self.grid = CutoutGrid(self.source)

        self.cutout00 = Mock(spec=SourceCutout)
        self.cutout01 = Mock(spec=SourceCutout)
        self.cutout10 = Mock(spec=SourceCutout)
        self.cutout11 = Mock(spec=SourceCutout)

        self.grid.add_cutout(self.cutout00, 0, 0)
        self.grid.add_cutout(self.cutout01, 0, 1)
        self.grid.add_cutout(self.cutout10, 1, 0)
        self.grid.add_cutout(self.cutout11, 1, 1)

    def test_added_cutouts_in_correct_location(self):
        assert_that(self.grid.get_cutout(0, 0), equal_to(self.cutout00))
        assert_that(self.grid.get_cutout(0, 1), equal_to(self.cutout01))
        assert_that(self.grid.get_cutout(1, 0), equal_to(self.cutout10))
        assert_that(self.grid.get_cutout(1, 1), equal_to(self.cutout11))

    def test_apply_function(self):
        function = Mock()

        self.grid.apply(function)

        assert_that(function.call_args_list, contains(
            call(self.cutout00, 0, 0),
            call(self.cutout01, 0, 1),
            call(self.cutout10, 1, 0),
            call(self.cutout11, 1, 1)))


if __name__ == '__main__':
    unittest.main()

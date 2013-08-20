__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock
from hamcrest import assert_that, equal_to

from ossos import astrom
from ossos.downloads.cutouts.grid import CutoutGrid
from ossos.downloads.cutouts.source import SourceCutout


class CutoutGridTest(unittest.TestCase):
    def setUp(self):
        self.source = Mock(spec=astrom.Source)
        self.source.num_readings.return_value = 2

    def test_added_cutouts_in_correct_location(self):
        grid = CutoutGrid(self.source)

        cutout00 = Mock(spec=SourceCutout)
        cutout01 = Mock(spec=SourceCutout)
        cutout10 = Mock(spec=SourceCutout)
        cutout11 = Mock(spec=SourceCutout)

        grid.add_cutout(cutout00, 0, 0)
        grid.add_cutout(cutout01, 0, 1)
        grid.add_cutout(cutout10, 1, 0)
        grid.add_cutout(cutout11, 1, 1)

        assert_that(grid.get_cutout(0, 0), equal_to(cutout00))
        assert_that(grid.get_cutout(0, 1), equal_to(cutout01))
        assert_that(grid.get_cutout(1, 0), equal_to(cutout10))
        assert_that(grid.get_cutout(1, 1), equal_to(cutout11))


if __name__ == '__main__':
    unittest.main()

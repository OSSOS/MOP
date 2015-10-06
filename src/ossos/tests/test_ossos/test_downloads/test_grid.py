__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, call
from hamcrest import assert_that, equal_to, contains

from ossos import astrom
from ossos.downloads.cutouts.grid import CutoutGrid
from ossos.downloads.cutouts.source import SourceCutout


class CutoutGridPrefilledTest(unittest.TestCase):
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

    def test_reset_source_location(self):
        self.grid.reset_source_location()

        self.cutout00.reset_source_location.called_once_with()
        self.cutout01.reset_source_location.called_once_with()
        self.cutout10.reset_source_location.called_once_with()
        self.cutout11.reset_source_location.called_once_with()


class CutoutGridTest(unittest.TestCase):
    def test_is_filled(self):
        source = Mock(spec=astrom.Source)
        source.num_readings.return_value = 2

        grid = CutoutGrid(source)

        def add_cutout(frame_index, time_index):
            grid.add_cutout(Mock(spec=SourceCutout), frame_index, time_index)

        assert_that(grid.is_filled(), equal_to(False))

        add_cutout(0, 0)
        assert_that(grid.is_filled(), equal_to(False))
        add_cutout(1, 0)
        assert_that(grid.is_filled(), equal_to(False))
        add_cutout(1, 1)
        assert_that(grid.is_filled(), equal_to(False))
        add_cutout(0, 1)
        assert_that(grid.is_filled(), equal_to(True))


if __name__ == '__main__':
    unittest.main()

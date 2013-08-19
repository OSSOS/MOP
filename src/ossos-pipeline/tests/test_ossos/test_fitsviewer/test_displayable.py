__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import matplotlib.pyplot as plt
from hamcrest import assert_that, equal_to, has_length, contains
from mock import Mock

from ossos import astrom
from ossos.downloads.cutouts.source import SourceCutout
from ossos.downloads.cutouts.grid import CutoutGrid
from ossos.fitsviewer.colormap import clip
from ossos.fitsviewer.displayable import DisplayableImageSinglet, DisplayableImageTriplet


class DisplayableImageSingletTest(unittest.TestCase):
    def setUp(self):
        mainhdu = Mock()
        mainhdu.data.shape = (100, 100)
        self.hdulist = [mainhdu]
        self.displayable = DisplayableImageSinglet(self.hdulist)

        fig = plt.figure()
        axes = plt.Axes(fig, [0, 0, 1, 1])
        self.displayable.axes = axes

    def test_draw_one_circle(self):
        axes = self.displayable.axes

        assert_that(axes.patches, has_length(0))
        cx = 1
        cy = 2
        cr = 3
        self.displayable.place_marker(cx, cy, cr)

        assert_that(axes.patches, has_length(1))
        circle = axes.patches[0]

        assert_that(circle.center, equal_to((cx, cy)))
        assert_that(circle.radius, equal_to(cr))

    def test_draw_second_circle_removes_first(self):
        axes = self.displayable.axes

        c1x = 1
        c1y = 2
        c1r = 3
        self.displayable.place_marker(c1x, c1y, c1r)

        assert_that(axes.patches, has_length(1))

        c2x = 4
        c2y = 5
        c2r = 6
        self.displayable.place_marker(c2x, c2y, c2r)

        assert_that(axes.patches, has_length(1))

        circle = axes.patches[0]

        assert_that(circle.center, equal_to((c2x, c2y)))
        assert_that(circle.radius, equal_to(c2r))


class DisplayableImageTripletTest(unittest.TestCase):
    def setUp(self):
        source = Mock(spec=astrom.Source)
        source.num_readings.return_value = 3

        grid = CutoutGrid(source)
        self.hdulist00 = Mock()
        self.hdulist01 = Mock()
        self.hdulist02 = Mock()
        self.hdulist10 = Mock()
        self.hdulist11 = Mock()
        self.hdulist12 = Mock()
        self.hdulist20 = Mock()
        self.hdulist21 = Mock()
        self.hdulist22 = Mock()

        def mock_cutout(hdulist):
            cutout = Mock(spec=SourceCutout)
            cutout.hdulist = hdulist
            return cutout

        grid.add_cutout(mock_cutout(self.hdulist00), 0, 0)
        grid.add_cutout(mock_cutout(self.hdulist01), 0, 1)
        grid.add_cutout(mock_cutout(self.hdulist02), 0, 2)
        grid.add_cutout(mock_cutout(self.hdulist10), 1, 0)
        grid.add_cutout(mock_cutout(self.hdulist11), 1, 1)
        grid.add_cutout(mock_cutout(self.hdulist12), 1, 2)
        grid.add_cutout(mock_cutout(self.hdulist20), 2, 0)
        grid.add_cutout(mock_cutout(self.hdulist21), 2, 1)
        grid.add_cutout(mock_cutout(self.hdulist22), 2, 2)

        self.grid = grid

    def test_frames_have_correct_hdulists(self):
        displayable = DisplayableImageTriplet(self.grid)

        assert_that(displayable.frames[0].hdulists,
                    contains(self.hdulist00, self.hdulist01, self.hdulist02))
        assert_that(displayable.frames[1].hdulists,
                    contains(self.hdulist10, self.hdulist11, self.hdulist12))
        assert_that(displayable.frames[2].hdulists,
                    contains(self.hdulist20, self.hdulist21, self.hdulist22))


class UtilityTest(unittest.TestCase):
    def test_clip_in_range(self):
        assert_that(clip(0.5, 0, 1), equal_to(0.5))

    def test_clip_below_range(self):
        assert_that(clip(-0.5, 0, 1), equal_to(0.0))

    def test_clip_above_range(self):
        assert_that(clip(1.5, 0, 1), equal_to(1.0))


if __name__ == '__main__':
    unittest.main()

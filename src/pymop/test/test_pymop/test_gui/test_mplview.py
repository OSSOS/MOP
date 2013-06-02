from pymop.gui import imgviewer

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import wx

import numpy as np
from hamcrest import assert_that, equal_to, has_length


class MPLViewTest(unittest.TestCase):
    def setUp(self):
        self.app = wx.App()
        self.rootframe = wx.Frame(None)

        self.viewer = imgviewer.MPLImageViewer(self.rootframe)

    def test_draw_one_circle(self):
        axes = self.viewer.axes

        assert_that(axes.patches, has_length(0))
        cx = 1
        cy = 2
        cr = 3
        self.viewer.draw_circle(cx, cy, cr)

        assert_that(axes.patches, has_length(1))
        circle = axes.patches[0]

        assert_that(circle.center, equal_to((cx, cy)))
        assert_that(circle.radius, equal_to(cr))

    def test_draw_second_circle_removes_first(self):
        axes = self.viewer.axes

        c1x = 1
        c1y = 2
        c1r = 3
        self.viewer.draw_circle(c1x, c1y, c1r)

        assert_that(axes.patches, has_length(1))

        c2x = 4
        c2y = 5
        c2r = 6
        self.viewer.draw_circle(c2x, c2y, c2r)

        assert_that(axes.patches, has_length(1))

        circle = axes.patches[0]

        assert_that(circle.center, equal_to((c2x, c2y)))
        assert_that(circle.radius, equal_to(c2r))


class UtilityTest(unittest.TestCase):
    def test_normalize_array(self):
        arr = np.array([[100, 20], [0, 60]])
        norm = imgviewer.normalize(arr, 5, 10)

        assert_that(norm[0, 0], equal_to(10))
        assert_that(norm[0, 1], equal_to(6))
        assert_that(norm[1, 0], equal_to(5))
        assert_that(norm[1, 1], equal_to(8))

    @unittest.skip("TODO: handle negative values")
    def test_normalize_array_negatives(self):
        arr = np.array([[100, -60], [-100, 60]])

        norm = imgviewer.normalize(arr, 5, 10)

        assert_that(norm[0, 0], equal_to(10))
        assert_that(norm[0, 1], equal_to(6))
        assert_that(norm[1, 0], equal_to(5))
        assert_that(norm[1, 1], equal_to(8))


if __name__ == '__main__':
    unittest.main()

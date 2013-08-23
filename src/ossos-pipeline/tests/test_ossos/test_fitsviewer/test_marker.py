__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from ossos.fitsviewer.displayable import Marker


class MarkerTest(unittest.TestCase):
    def test_cross_location(self):
        x = 10
        y = 10
        radius = 6

        marker = Marker(x, y, radius)

        assert_that(marker.left_hair.get_xdata(), equal_to((4, 7)))
        assert_that(marker.left_hair.get_ydata(), equal_to((10, 10)))

        assert_that(marker.right_hair.get_xdata(), equal_to((13, 16)))
        assert_that(marker.right_hair.get_ydata(), equal_to((10, 10)))

        assert_that(marker.top_hair.get_xdata(), equal_to((10, 10)))
        assert_that(marker.top_hair.get_ydata(), equal_to((13, 16)))

        assert_that(marker.bottom_hair.get_xdata(), equal_to((10, 10)))
        assert_that(marker.bottom_hair.get_ydata(), equal_to((4, 7)))

    def test_move_marker_moves_circle_and_cross(self):
        x = 10
        y = 10
        radius = 6

        marker = Marker(x, y, radius)

        new_x = 20
        new_y = 30
        marker.center = (new_x, new_y)

        assert_that(marker.circle.center, equal_to((new_x, new_y)))

        assert_that(marker.left_hair.get_xdata(), equal_to((14, 17)))
        assert_that(marker.left_hair.get_ydata(), equal_to((30, 30)))

        assert_that(marker.right_hair.get_xdata(), equal_to((23, 26)))
        assert_that(marker.right_hair.get_ydata(), equal_to((30, 30)))

        assert_that(marker.top_hair.get_xdata(), equal_to((20, 20)))
        assert_that(marker.top_hair.get_ydata(), equal_to((33, 36)))

        assert_that(marker.bottom_hair.get_xdata(), equal_to((20, 20)))
        assert_that(marker.bottom_hair.get_ydata(), equal_to((24, 27)))

    def test_change_radius(self):
        x = 20
        y = 20
        radius = 6

        marker = Marker(x, y, radius)

        new_radius = 12
        marker.radius = new_radius

        assert_that(marker.circle.radius, equal_to(new_radius))

        assert_that(marker.left_hair.get_xdata(), equal_to((8, 14)))
        assert_that(marker.left_hair.get_ydata(), equal_to((20, 20)))

        assert_that(marker.right_hair.get_xdata(), equal_to((26, 32)))
        assert_that(marker.right_hair.get_ydata(), equal_to((20, 20)))

        assert_that(marker.top_hair.get_xdata(), equal_to((20, 20)))
        assert_that(marker.top_hair.get_ydata(), equal_to((26, 32)))

        assert_that(marker.bottom_hair.get_xdata(), equal_to((20, 20)))
        assert_that(marker.bottom_hair.get_ydata(), equal_to((8, 14)))

    def test_toggle_reticule(self):
        marker = Marker(10, 10, 10)

        def assert_visible(visible):
            assert_that(marker.circle.get_visible(), equal_to(visible))
            for line in marker.lines:
                assert_that(line.get_visible(), equal_to(visible))

        assert_visible(True)

        marker.toggle_reticule()

        assert_visible(False)

        marker.toggle_reticule()

        assert_visible(True)


if __name__ == '__main__':
    unittest.main()

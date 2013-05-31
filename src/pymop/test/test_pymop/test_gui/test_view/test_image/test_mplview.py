__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import numpy as np
from hamcrest import assert_that, equal_to

from pymop.gui.view.image import mplview


class MPLViewTest(unittest.TestCase):
    def test_normalize_array(self):
        arr = np.array([[100, 20], [0, 60]])
        norm = mplview.normalize(arr, 5, 10)

        assert_that(norm[0, 0], equal_to(10))
        assert_that(norm[0, 1], equal_to(6))
        assert_that(norm[1, 0], equal_to(5))
        assert_that(norm[1, 1], equal_to(8))

    @unittest.skip("TODO: handle negative values")
    def test_normalize_array_negatives(self):
        arr = np.array([[100, -60], [-100, 60]])

        norm = mplview.normalize(arr, 5, 10)

        assert_that(norm[0, 0], equal_to(10))
        assert_that(norm[0, 1], equal_to(6))
        assert_that(norm[1, 0], equal_to(5))
        assert_that(norm[1, 1], equal_to(8))


if __name__ == '__main__':
    unittest.main()

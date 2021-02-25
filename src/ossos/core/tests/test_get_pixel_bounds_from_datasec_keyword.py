from unittest import TestCase

__author__ = 'jjk'

import util

class TestGet_pixel_bounds_from_datasec_keyword(TestCase):

    def test_get_pixel_bounds_from_datasec_keyword(self):
        """Check that correct tuple is returned"""
        self.assertEqual(util.get_pixel_bounds_from_datasec_keyword("[2:2040,2:4009]"), ((2, 2040), (2, 4009)))

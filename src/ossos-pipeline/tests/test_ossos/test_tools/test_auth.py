__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import Mock

from ossos import auth


class AuthTest(unittest.TestCase):
    def test_get_cadc_username_trims_keycode(self):
        auth.get_issuer_common_name = Mock(return_value="drusk_bc4")
        assert_that(auth.get_cadc_username(), equal_to("drusk"))

    def test_get_cadc_username_contains_underscores(self):
        auth.get_issuer_common_name = Mock(return_value="d_rusk_bc4")
        assert_that(auth.get_cadc_username(), equal_to("d_rusk"))

    def test_get_cadc_username_no_keycode(self):
        auth.get_issuer_common_name = Mock(return_value="drusk")
        assert_that(auth.get_cadc_username(), equal_to("drusk"))


if __name__ == '__main__':
    unittest.main()

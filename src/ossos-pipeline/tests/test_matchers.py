__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from tests.matchers import round_sigfigs, almost_equal


class MatchersTest(unittest.TestCase):
    def test_round_sigfigs(self):
        assert_that(round_sigfigs(1234, 3), equal_to(1230.0))

    def test_round_sigfigs_neg(self):
        assert_that(round_sigfigs(-1234, 3), equal_to(-1230.0))

    def test_round_sigfigs_more_than_input(self):
        assert_that(round_sigfigs(1234, 7), equal_to(1234.000))

    def test_assert_almost_equal(self):
        assert_that(1.234, almost_equal(1.23444, 4))

    def test_assert_almost_equal2(self):
        assert_that(1.234012345, almost_equal(1.23403876, 5))
        # NOTE: not 5 sig figs because of the rounding.  Is this how it should be?
        assert_that(1.234012345, almost_equal(1.23409876, 4))


if __name__ == '__main__':
    unittest.main()

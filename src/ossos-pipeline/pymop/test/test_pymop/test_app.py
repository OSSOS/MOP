__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains_inanyorder

from test.base_tests import FileReadingTestCase
from pymop.app import listdir_for_suffix


class UtilityTest(FileReadingTestCase):
    def test_listdir_for_suffix(self):
        dir = self.get_abs_path("data/testdir")

        listing1 = listdir_for_suffix(dir, "cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = listdir_for_suffix(dir, "reals.astrom")
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))


if __name__ == '__main__':
    unittest.main()

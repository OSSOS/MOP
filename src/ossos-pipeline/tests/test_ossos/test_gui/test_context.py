__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains_inanyorder, equal_to

from tests.base_tests import FileReadingTestCase
from ossos.gui.context import LocalDirectoryWorkingContext


class LocalDirectoryContextTest(FileReadingTestCase):
    def test_directory_manager_get_listing(self):
        directory = self.get_abs_path("data/testdir")

        directory_manager = LocalDirectoryWorkingContext(directory)

        listing1 = directory_manager.get_listing("cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = directory_manager.get_listing("reals.astrom")
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))

        assert_that(directory_manager.get_full_path("xxx1.cands.astrom"),
                    equal_to(self.get_abs_path("data/testdir/xxx1.cands.astrom")))


if __name__ == '__main__':
    unittest.main()

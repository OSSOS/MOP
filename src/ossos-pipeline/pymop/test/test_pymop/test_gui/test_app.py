__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains_inanyorder, equal_to

from test.base_tests import FileReadingTestCase
from pymop.gui import tasks
from pymop.gui.app import listdir_for_suffix, DirectoryContext


class DirectoryContextTest(FileReadingTestCase):
    def test_listdir_for_suffix(self):
        directory = self.get_abs_path("data/testdir")

        listing1 = listdir_for_suffix(directory, "cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = listdir_for_suffix(directory, "reals.astrom")
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))

    def test_listdir_for_task(self):
        directory = self.get_abs_path("data/testdir")

        listing1 = listdir_for_suffix(directory, tasks.get_suffix(tasks.CANDS_TASK))
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = listdir_for_suffix(directory, tasks.get_suffix(tasks.REALS_TASK))
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))

    def test_directory_manager_get_listing(self):
        directory = self.get_abs_path("data/testdir")

        directory_manager = DirectoryContext(directory)

        listing1 = directory_manager.get_listing("cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        assert_that(directory_manager.get_full_path("xxx1.cands.astrom"),
                    equal_to(self.get_abs_path("data/testdir/xxx1.cands.astrom")))


if __name__ == '__main__':
    unittest.main()

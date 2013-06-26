__author__ = "David Rusk <drusk@uvic.ca>"

import copy
import unittest

from hamcrest import assert_that, is_in, is_not, equal_to, contains_inanyorder
from mock import Mock

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io import workload
from pymop.io.workload import (WorkUnitFactory, DirectoryManager,
                               NoAvailableWorkException)
from pymop.io.astrom import AstromParser


class TestProgressManager(object):
    """
    A partial test implementation of the ProgressManager interface which
    just remembers values set on it without persisting them to disk in
    any way.
    """

    def __init__(self):
        self.done = []

    def is_done(self, filename):
        return filename in self.done

    def record_done(self, filename):
        self.done.append(filename)


class WorkUnitFactoryTest(unittest.TestCase):
    def test_create_workload_fresh_directory(self):
        file1 = "file1"
        file2 = "file2"
        test_files = [file1, file2]

        def _get_listing():
            return copy.deepcopy(test_files)

        directory_manager = Mock(spec=DirectoryManager)
        directory_manager.get_listing = _get_listing

        progress_manager = TestProgressManager()
        parser = Mock(spec=AstromParser)

        undertest = WorkUnitFactory(directory_manager, progress_manager, parser)

        workunit1 = undertest.create_workunit()
        assert_that(workunit1.get_filename(), is_in(test_files))
        progress_manager.record_done(workunit1.get_filename())

        workunit2 = undertest.create_workunit()
        assert_that(workunit2.get_filename(), is_in(test_files))
        assert_that(workunit2.get_filename(),
                    is_not(equal_to(workunit1.get_filename())))
        progress_manager.record_done(workunit2.get_filename())

        self.assertRaises(NoAvailableWorkException, undertest.create_workunit)


class DirectoryManagerTest(FileReadingTestCase):
    def test_listdir_for_suffix(self):
        dir = self.get_abs_path("data/testdir")

        listing1 = workload.listdir_for_suffix(dir, "cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = workload.listdir_for_suffix(dir, "reals.astrom")
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))

    def test_listdir_for_task(self):
        dir = self.get_abs_path("data/testdir")

        listing1 = workload.listdir_for_suffix(dir, tasks.get_suffix(tasks.CANDS_TASK))
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        listing2 = workload.listdir_for_suffix(dir, tasks.get_suffix(tasks.REALS_TASK))
        assert_that(listing2, contains_inanyorder("xxx1.reals.astrom", "xxx2.reals.astrom"))

    def test_directory_manager_get_listing(self):
        dir = self.get_abs_path("data/testdir")

        directory_manager = DirectoryManager(dir)

        listing1 = directory_manager.get_listing("cands.astrom")
        assert_that(listing1, contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))

        assert_that(directory_manager.get_full_path("xxx1.cands.astrom"),
                    equal_to(self.get_abs_path("data/testdir/xxx1.cands.astrom")))


if __name__ == '__main__':
    unittest.main()

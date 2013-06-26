__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, is_in, is_not, equal_to, contains_inanyorder
from mock import Mock

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io import workload
from pymop.io.persistence import InMemoryProgressManager
from pymop.io.workload import (WorkUnitFactory, DirectoryManager,
                               NoAvailableWorkException)
from pymop.io.astrom import AstromParser


class TestDirectoryManager(object):
    def __init__(self):
        self.listings = {}

    def set_listing(self, suffix, listing):
        self.listings[suffix] = listing[:]

    def get_listing(self, suffix):
        return self.listings[suffix]

    def get_full_path(self, filename):
        return filename


class WorkUnitFactoryTest(unittest.TestCase):
    def test_create_workload_fresh_directory(self):
        taskid = "id"
        test_files = ["file1", "file2"]

        directory_manager = TestDirectoryManager()
        directory_manager.set_listing(taskid, test_files)

        progress_manager = InMemoryProgressManager(directory_manager)
        parser = Mock(spec=AstromParser)

        undertest = WorkUnitFactory(taskid, directory_manager, progress_manager, parser)

        workunit1 = undertest.create_workunit()
        assert_that(workunit1.get_filename(), is_in(test_files))

        progress_manager.lock(workunit1.get_filename())
        progress_manager.record_done(workunit1.get_filename())
        progress_manager.unlock(workunit1.get_filename())

        workunit2 = undertest.create_workunit()
        assert_that(workunit2.get_filename(), is_in(test_files))
        assert_that(workunit2.get_filename(),
                    is_not(equal_to(workunit1.get_filename())))

        progress_manager.lock(workunit2.get_filename())
        progress_manager.record_done(workunit2.get_filename())
        progress_manager.unlock(workunit2.get_filename())

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

__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import assert_that, is_in, is_not, equal_to, contains_inanyorder
from mock import Mock

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io import workload
from pymop.io.astrom import AstromParser
from pymop.io.persistence import InMemoryProgressManager
from pymop.io.workload import (WorkUnitProvider, DirectoryManager,
                               WorkUnit, RealsWorkUnit, CandidatesWorkUnit,
                               DataCollection, NoAvailableWorkException)


class TestDirectoryManager(object):
    def __init__(self):
        self.listings = {}

    def set_listing(self, suffix, listing):
        self.listings[suffix] = listing[:]

    def get_listing(self, suffix):
        return self.listings[suffix]

    def get_full_path(self, filename):
        return filename


class TestWorkUnitBuilder(object):
    def build_workunit(self, full_path):
        _, filename = os.path.split(full_path)
        workunit = Mock(spec=WorkUnit)
        workunit.get_filename.return_value = filename
        return workunit


class WorkUnitFactoryTest(unittest.TestCase):
    def setUp(self):
        self.taskid = "id"
        self.file1 = "file1"
        self.file2 = "file2"
        self.test_files = [self.file1, self.file2]

        directory_manager = TestDirectoryManager()
        progress_manager = InMemoryProgressManager(directory_manager)
        builder = TestWorkUnitBuilder()

        self.undertest = WorkUnitProvider(self.taskid, directory_manager,
                                         progress_manager, builder)
        self.directory_manager = directory_manager
        self.progress_manager = progress_manager
        self.directory_manager.set_listing(self.taskid, self.test_files)

    def test_create_workload_acquires_lock(self):
        self.directory_manager.set_listing(self.taskid, self.test_files)
        workunit1 = self.undertest.get_workunit()
        assert_that(self.progress_manager.owns_lock(workunit1.get_filename()),
                    equal_to(True))

    def test_create_workload_fresh_directory(self):
        workunit1 = self.undertest.get_workunit()
        assert_that(workunit1.get_filename(), is_in(self.test_files))
        self.progress_manager.record_done(workunit1.get_filename())

        workunit2 = self.undertest.get_workunit()
        assert_that(workunit2.get_filename(), is_in(self.test_files))
        assert_that(workunit2.get_filename(),
                    is_not(equal_to(workunit1.get_filename())))
        self.progress_manager.record_done(workunit2.get_filename())

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_create_workload_one_file_already_done(self):
        self.progress_manager.done.append(self.file1)

        workunit = self.undertest.get_workunit()
        assert_that(workunit.get_filename(), equal_to(self.file2))
        self.progress_manager.record_done(self.file2)

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_create_workload_locked_files(self):
        self.progress_manager.add_external_lock(self.file2)

        workunit = self.undertest.get_workunit()
        assert_that(workunit.get_filename(), equal_to(self.file1))
        self.progress_manager.record_done(self.file1)

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)


class AbstractWorkUnitTest(FileReadingTestCase):
    def setUp(self):
        self.testfile = "data/1584431p15.measure3.cands.astrom"
        parser = AstromParser()
        astrom_data = parser.parse(self.get_abs_path(self.testfile))

        self.data_collection = DataCollection(astrom_data)


class WorkUnitTest(AbstractWorkUnitTest):
    def setUp(self):
        super(WorkUnitTest, self).setUp()

        self.workunit = WorkUnit(self.testfile, self.data_collection)

    def test_initialization(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_source_count(), equal_to(3))
        assert_that(self.workunit.get_obs_count(), equal_to(3))

    def test_next_source_previous_source(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_next_source_wrap(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(2))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_next_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_previous_source_wrap(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(2))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_previous_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_next_source_resets_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))


class RealsWorkUnitTest(AbstractWorkUnitTest):
    def setUp(self):
        super(RealsWorkUnitTest, self).setUp()

        self.workunit = RealsWorkUnit(self.data_collection)

    @unittest.skip("TODO")
    def test_accept_current_item(self):
        pass

    @unittest.skip("TODO")
    def test_reject_current_item(self):
        pass


class CandidatesWorkUnitTest(AbstractWorkUnitTest):
    def setUp(self):
        super(CandidatesWorkUnitTest, self).setUp()

        self.workunit = CandidatesWorkUnit(self.data_collection)

    @unittest.skip("TODO")
    def test_accept_current_item(self):
        pass

    @unittest.skip("TODO")
    def test_reject_current_item(self):
        pass


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

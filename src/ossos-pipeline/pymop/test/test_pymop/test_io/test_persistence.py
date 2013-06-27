__author__ = "David Rusk <drusk@uvic.ca>"

import getpass
import unittest

from mock import patch
from hamcrest import (assert_that, contains_inanyorder, has_length, contains,
                      equal_to)

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io.persistence import (ProgressManager, FileLockedException,
                                  RequiresLockException, LOCK_SUFFIX)
from pymop.io.astrom import AstromWorkload

WD_HAS_PROGRESS = "data/persistence_has_progress"
WD_NO_LOG = "data/persistence_no_log"


class ProgressManagerLoadingTest(FileReadingTestCase):
    def setUp(self):
        self.working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        self.progress_manager = ProgressManager(self.working_directory)

    def tearDown(self):
        self.progress_manager.clean(suffixes=[LOCK_SUFFIX])

    def test_load_progress(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))

    def test_astrom_workload_filtered_reals(self):
        workload = AstromWorkload(self.working_directory, self.progress_manager,
                                  tasks.REALS_TASK)

        expected_filenames = ["xxx1.reals.astrom", "xxx2.reals.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))

    def test_astrom_workload_filtered_cands(self):
        workload = AstromWorkload(self.working_directory, self.progress_manager,
                                  tasks.CANDS_TASK)

        expected_filenames = ["xxx2.cands.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))


class ProgressManagerFreshDirectoryTest(FileReadingTestCase):
    def setUp(self):
        self.working_directory = self.get_abs_path(WD_NO_LOG)
        self.progress_manager = ProgressManager(self.working_directory)

    def tearDown(self):
        # Get rid of generated files so we don't interfere with other tests
        self.progress_manager.clean()

    def test_load_progress_no_logs(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    has_length(0))

    def test_record_done_requires_lock(self):
        self.assertRaises(
            RequiresLockException,
            self.progress_manager.record_done, "xxx2.reals.astrom")

    def test_write_progress(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.progress_manager.lock(processed1)
        self.progress_manager.record_done(processed1)

        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains(processed1))
        assert_that(self.progress_manager.is_done(processed1), equal_to(True))

        # Create a second persistence manager and make sure it sees the changes
        manager2 = ProgressManager(self.working_directory)
        assert_that(manager2.get_done(tasks.CANDS_TASK), has_length(0))
        assert_that(manager2.get_done(tasks.REALS_TASK), contains(processed1))
        assert_that(manager2.is_done(processed1), equal_to(True))

    def test_write_progress_two_simultaneous_managers(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.progress_manager.lock(processed1)
        self.progress_manager.record_done(processed1)

        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains(processed1))

        # Create a second simultaneous manager
        manager2 = ProgressManager(self.working_directory)
        processed2 = "xxx3.reals.astrom"
        self.progress_manager.lock(processed2)
        manager2.record_done(processed2)

        # Make sure second manager sees both entries
        assert_that(manager2.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(manager2.get_done(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))

        # Make sure original manager sees both entries
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))

    def test_lock_file(self):
        file1 = "xxx1.cands.astrom"
        self.progress_manager.lock(file1)

        # No-one else should be able to acquire the lock...
        manager2 = ProgressManager(self.working_directory)
        self.assertRaises(FileLockedException, manager2.lock, file1)

        # ... until we unlock it
        self.progress_manager.unlock(file1)
        manager2.lock(file1)

    @patch.object(getpass, "getuser")
    def test_lock_has_locker_id(self, getuser_mock):
        lock_holding_user = "lock_holding_user"
        lock_requesting_user = "lock_requesting_user"

        getuser_mock.return_value = lock_holding_user
        file1 = "xxx1.cands.astrom"
        self.progress_manager.lock(file1)

        manager2 = ProgressManager(self.working_directory)

        try:
            getuser_mock.return_value = lock_requesting_user
            manager2.lock(file1)
            self.fail("Should have thrown FileLockedExcecption")
        except FileLockedException as ex:
            assert_that(ex.filename, equal_to(file1))
            assert_that(ex.locker, equal_to(lock_holding_user))

    def test_record_index_requires_lock(self):
        self.assertRaises(RequiresLockException,
                          self.progress_manager.record_index,
                          "xxx1.cands.astrom", 0)

    def test_record_index(self):
        file1 = "xxx1.cands.astrom"
        self.progress_manager.lock(file1)
        self.progress_manager.record_index(file1, 1)
        self.progress_manager.record_index(file1, 3)
        self.progress_manager.record_index(file1, 0)

        assert_that(self.progress_manager.get_processed_indices(file1),
                    contains_inanyorder(1, 3, 0))

        # Check they are still recorded after we release lock
        self.progress_manager.unlock(file1)
        assert_that(self.progress_manager.get_processed_indices(file1),
                    contains_inanyorder(1, 3, 0))

        # Check other clients can read them
        manager2 = ProgressManager(self.working_directory)
        assert_that(manager2.get_processed_indices(file1),
                    contains_inanyorder(1, 3, 0))

    def test_unlock_after_record_done_no_error(self):
        file1 = "xxx1.cands.astrom"
        self.progress_manager.lock(file1)
        self.progress_manager.record_done(file1)
        self.progress_manager.unlock(file1)

    def test_record_done_does_not_unlock_all(self):
        file1 = "xxx1.cands.astrom"
        file2 = "xxx2.cands.astrom"
        manager2 = ProgressManager(self.working_directory)

        self.progress_manager.lock(file1)
        manager2.lock(file2)

        self.progress_manager.record_done(file1)
        assert_that(manager2.owns_lock(file2), equal_to(True))

        manager2.unlock(file2)
        assert_that(manager2.owns_lock(file2), equal_to(False))

    def test_get_processed_indices_empty_should_not_cause_error(self):
        assert_that(self.progress_manager.get_processed_indices("xxx1.cands.astrom"),
                    has_length(0))

    def test_get_processed_indices_after_done(self):
        filename = "xxx1.cands.astrom"
        self.progress_manager.lock(filename)
        self.progress_manager.record_index(filename, 0)
        self.progress_manager.record_index(filename, 1)
        self.progress_manager.record_index(filename, 2)
        self.progress_manager.unlock(filename)

        assert_that(self.progress_manager.get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))

        self.progress_manager.lock(filename)
        self.progress_manager.record_done(filename)
        self.progress_manager.unlock(filename)

        assert_that(self.progress_manager.get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))

        # Double check with a second manager
        assert_that(ProgressManager(self.working_directory).get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))


if __name__ == '__main__':
    unittest.main()

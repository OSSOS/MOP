__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock
from hamcrest import (assert_that, contains_inanyorder, has_length, contains,
                      equal_to)

from tests.base_tests import FileReadingTestCase
from ossos.gui import tasks
from ossos.gui.context import LocalDirectoryWorkingContext
from ossos.gui.progress import (LocalProgressManager, InMemoryProgressManager,
                                   FileLockedException, RequiresLockException,
                                   LOCK_SUFFIX)

WD_HAS_PROGRESS = "data/persistence_has_progress"
WD_NO_LOG = "data/persistence_no_log"


class ProgressManagerLoadingTest(FileReadingTestCase):
    def setUp(self):
        self.working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        directory_manager = LocalDirectoryWorkingContext(self.working_directory)
        self.progress_manager = LocalProgressManager(directory_manager)

    def tearDown(self):
        self.progress_manager.clean(suffixes=[LOCK_SUFFIX])

    def test_load_progress(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))


class ProgressManagerFreshDirectoryTest(FileReadingTestCase):
    def setUp(self):
        self.main_user_id = "main_user"
        self.working_directory = self.get_abs_path(WD_NO_LOG)
        directory_manager = LocalDirectoryWorkingContext(self.working_directory)
        self.progress_manager = LocalProgressManager(directory_manager,
                                                     userid=self.main_user_id)

    def create_concurrent_progress_manager(self, userid="test_user"):
        directory_manager = LocalDirectoryWorkingContext(self.working_directory)
        return LocalProgressManager(directory_manager, userid=userid)

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
        manager2 = self.create_concurrent_progress_manager()
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
        manager2 = self.create_concurrent_progress_manager()
        processed2 = "xxx3.reals.astrom"
        manager2.lock(processed2)
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
        lock_requesting_user = "lock_requesting_user"
        manager2 = self.create_concurrent_progress_manager(lock_requesting_user)
        self.assertRaises(FileLockedException, manager2.lock, file1)

        # ... until we unlock it
        self.progress_manager.unlock(file1)
        manager2.lock(file1)

    def test_lock_has_locker_id(self):
        lock_holding_user = self.main_user_id
        lock_requesting_user = "lock_requesting_user"

        file1 = "xxx1.cands.astrom"
        self.progress_manager.lock(file1)

        manager2 = self.create_concurrent_progress_manager(lock_requesting_user)

        try:
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
        manager2 = self.create_concurrent_progress_manager()
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
        manager2 = self.create_concurrent_progress_manager()

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
        assert_that(self.create_concurrent_progress_manager().get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))


class InMemoryProgressManagerTest(unittest.TestCase):
    def setUp(self):
        self.file1 = "file1"
        self.file2 = "file2"

        directory_manager = Mock(spec=LocalDirectoryWorkingContext)
        self.undertest = InMemoryProgressManager(directory_manager)

    def test_done(self):
        assert_that(self.undertest.is_done(self.file1), equal_to(False))
        assert_that(self.undertest.is_done(self.file2), equal_to(False))

        self.undertest.lock(self.file2)
        self.undertest.record_done(self.file2)
        self.undertest.unlock(self.file2)

        assert_that(self.undertest.is_done(self.file1), equal_to(False))
        assert_that(self.undertest.is_done(self.file2), equal_to(True))

        self.undertest.lock(self.file1)
        self.undertest.record_done(self.file1)
        self.undertest.unlock(self.file1)

        assert_that(self.undertest.is_done(self.file1), equal_to(True))
        assert_that(self.undertest.is_done(self.file2), equal_to(True))

    def test_processed_indices(self):
        assert_that(self.undertest.get_processed_indices(self.file1),
                    has_length(0))
        self.undertest.lock(self.file1)
        self.undertest.record_index(self.file1, 1)
        assert_that(self.undertest.get_processed_indices(self.file1),
                    contains_inanyorder(1))

        self.undertest.record_index(self.file1, 2)
        assert_that(self.undertest.get_processed_indices(self.file1),
                    contains_inanyorder(1, 2))

        assert_that(self.undertest.get_processed_indices(self.file2),
                    has_length(0))

    def test_locking(self):
        assert_that(self.undertest.owns_lock(self.file1), equal_to(False))
        assert_that(self.undertest.owns_lock(self.file2), equal_to(False))

        self.undertest.lock(self.file1)

        assert_that(self.undertest.owns_lock(self.file1), equal_to(True))
        assert_that(self.undertest.owns_lock(self.file2), equal_to(False))

        self.assertRaises(RequiresLockException, self.undertest.record_done, self.file2)
        self.assertRaises(RequiresLockException, self.undertest.record_index, self.file2, 1)

    def test_external_locks(self):
        self.undertest.add_external_lock(self.file1)

        assert_that(self.undertest.owns_lock(self.file1), equal_to(False))
        self.assertRaises(FileLockedException, self.undertest.lock, self.file1)
        self.assertRaises(FileLockedException, self.undertest.unlock, self.file1)

        self.undertest.lock(self.file2)
        assert_that(self.undertest.owns_lock(self.file2), equal_to(True))


if __name__ == '__main__':
    unittest.main()

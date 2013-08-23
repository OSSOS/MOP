__author__ = "David Rusk <drusk@uvic.ca>"

import getpass
import unittest

from hamcrest import assert_that, equal_to, contains, contains_inanyorder, has_length
from mock import patch

from tests.base_tests import FileReadingTestCase
from ossos import storage
from ossos.gui.context import VOSpaceWorkingContext
from ossos.gui import progress
from ossos.gui.progress import VOSpaceProgressManager, FileLockedException, RequiresLockException

BASE_TEST_DIR = "vos:OSSOS/integration_tests/"
PROTOTYPE_FILE = "data/prototype"
PERSISTENCE_TEST_DIR = BASE_TEST_DIR + "persistence_tests"

TEST_FILE_1 = "1.cands.astrom"
TEST_FILE_2 = "2.reals.astrom"
TEST_FILE_3 = "3.cands.astrom"

TEST_USER = "testuser"


class AbstractVOSpaceProgressManagerTestCase(object):
    # Do not run this class directly; run its subclasses

    def create_vofile(self, destination):
        # Just copy a prototype file until I figure out how to do this
        # properly
        storage.copy(self.get_abs_path(PROTOTYPE_FILE), destination)

    def setUp(self):
        self.main_user_id = "main_user"
        self.context = VOSpaceWorkingContext(PERSISTENCE_TEST_DIR)
        self.undertest = VOSpaceProgressManager(
            self.context, userid=self.main_user_id,
            track_partial_progress=self._tracks_partial_progress())

        self.create_vofile(self.context.get_full_path(TEST_FILE_1))
        self.create_vofile(self.context.get_full_path(TEST_FILE_2))
        self.create_vofile(self.context.get_full_path(TEST_FILE_3))

    def get_uri(self, filename):
        return self.context.get_full_path(filename)

    def get_property(self, filename, property):
        return storage.get_property(self.get_uri(filename), property)

    def has_property(self, filename, property):
        return storage.has_property(self.get_uri(filename), property)

    def tearDown(self):
        for filename in self.context.listdir():
            storage.delete_uri(self.context.get_full_path(filename))

    def create_independent_manager(self, userid="test_user"):
        return VOSpaceProgressManager(
            self.context, userid=userid,
            track_partial_progress=self._tracks_partial_progress())

    def _tracks_partial_progress(self):
        raise NotImplementedError()

    def test_is_done_record_done(self):
        assert_that(self.undertest.is_done(TEST_FILE_1), equal_to(False))
        assert_that(self.undertest.is_done(TEST_FILE_2), equal_to(False))
        assert_that(self.undertest.is_done(TEST_FILE_3), equal_to(False))

        self.undertest.lock(TEST_FILE_2)
        self.undertest.record_done(TEST_FILE_2)
        self.undertest.unlock(TEST_FILE_2)
        assert_that(self.undertest.is_done(TEST_FILE_1), equal_to(False))
        assert_that(self.undertest.is_done(TEST_FILE_2), equal_to(True))
        assert_that(self.undertest.is_done(TEST_FILE_3), equal_to(False))

        self.undertest.lock(TEST_FILE_3)
        self.undertest.record_done(TEST_FILE_3)
        self.undertest.unlock(TEST_FILE_3)
        assert_that(self.undertest.is_done(TEST_FILE_1), equal_to(False))
        assert_that(self.undertest.is_done(TEST_FILE_2), equal_to(True))
        assert_that(self.undertest.is_done(TEST_FILE_3), equal_to(True))

        self.undertest.lock(TEST_FILE_1)
        self.undertest.record_done(TEST_FILE_1)
        self.undertest.unlock(TEST_FILE_1)
        assert_that(self.undertest.is_done(TEST_FILE_1), equal_to(True))
        assert_that(self.undertest.is_done(TEST_FILE_2), equal_to(True))
        assert_that(self.undertest.is_done(TEST_FILE_3), equal_to(True))

    def test_get_done(self):
        assert_that(self.undertest.get_done(".cands.astrom"), contains())
        assert_that(self.undertest.get_done(".reals.astrom"), contains())

        self.undertest.lock(TEST_FILE_1)
        self.undertest.record_done(TEST_FILE_1)
        self.undertest.unlock(TEST_FILE_1)
        assert_that(self.undertest.get_done(".cands.astrom"), contains(TEST_FILE_1))
        assert_that(self.undertest.get_done(".reals.astrom"), contains())

        self.undertest.lock(TEST_FILE_2)
        self.undertest.record_done(TEST_FILE_2)
        self.undertest.unlock(TEST_FILE_2)
        assert_that(self.undertest.get_done(".cands.astrom"), contains(TEST_FILE_1))
        assert_that(self.undertest.get_done(".reals.astrom"), contains(TEST_FILE_2))

        self.undertest.lock(TEST_FILE_3)
        self.undertest.record_done(TEST_FILE_3)
        self.undertest.unlock(TEST_FILE_3)
        assert_that(self.undertest.get_done(".cands.astrom"),
                    contains_inanyorder(TEST_FILE_1, TEST_FILE_3))
        assert_that(self.undertest.get_done(".reals.astrom"), contains(TEST_FILE_2))

    def test_lock_file(self):
        lock_requesting_user = "lock_requesting_user"

        self.undertest.lock(TEST_FILE_1)

        # No-one else should be able to acquire the lock...
        manager2 = self.create_independent_manager(userid=lock_requesting_user)
        self.assertRaises(FileLockedException, manager2.lock, TEST_FILE_1)

        # ... until we unlock it
        self.undertest.unlock(TEST_FILE_1)
        manager2.lock(TEST_FILE_1)

    def test_lock_holder_no_file_locked_exception(self):
        lock_holding_user = self.main_user_id
        lock_requesting_user = "lock_requesting_user"

        self.undertest.lock(TEST_FILE_1)

        # No-one else should be able to acquire the lock...
        manager2 = self.create_independent_manager(userid=lock_requesting_user)
        self.assertRaises(FileLockedException, manager2.lock, TEST_FILE_1)

        # ... but we should be able to without getting a FileLockedException
        self.undertest.lock(TEST_FILE_1)

    def test_lock_has_locker_id(self):
        lock_holding_user = self.main_user_id
        lock_requesting_user = "lock_requesting_user"

        file1 = TEST_FILE_1
        self.undertest.lock(file1)

        manager2 = self.create_independent_manager(userid=lock_requesting_user)

        try:
            manager2.lock(file1)
            self.fail("Should have thrown FileLockedExcecption")
        except FileLockedException as ex:
            assert_that(ex.filename, equal_to(file1))
            assert_that(ex.locker, equal_to(lock_holding_user))

    def test_record_done_puts_username_in_property(self):
        filename = TEST_FILE_1
        self.undertest.lock(filename)
        self.undertest.record_done(filename)
        self.undertest.unlock(filename)

        assert_that(self.get_property(filename, progress.DONE_PROPERTY),
                    equal_to(self.main_user_id))

    def test_unlock_after_record_done_no_error(self):
        file1 = TEST_FILE_1
        self.undertest.lock(file1)
        self.undertest.record_done(file1)
        self.undertest.unlock(file1)

    def test_record_done_does_not_unlock_all(self):
        file1 = TEST_FILE_1
        file2 = TEST_FILE_3
        manager2 = VOSpaceProgressManager(self.context)

        self.undertest.lock(file1)
        manager2.lock(file2)

        self.undertest.record_done(file1)
        assert_that(manager2.owns_lock(file2), equal_to(True))

        manager2.unlock(file2)
        assert_that(manager2.owns_lock(file2), equal_to(False))


class PartialProgressTrackingTest(AbstractVOSpaceProgressManagerTestCase, FileReadingTestCase):
    def _tracks_partial_progress(self):
        return True

    def test_record_index_requires_lock(self):
        self.assertRaises(RequiresLockException,
                          self.undertest.record_index,
                          TEST_FILE_1, 0)

    def test_record_index(self):
        self.undertest.lock(TEST_FILE_1)
        self.undertest.record_index(TEST_FILE_1, 1)
        self.undertest.record_index(TEST_FILE_1, 3)
        self.undertest.record_index(TEST_FILE_1, 0)

        assert_that(self.undertest.get_processed_indices(TEST_FILE_1),
                    contains_inanyorder(1, 3, 0))

        # Check they are still recorded after we release lock
        self.undertest.unlock(TEST_FILE_1)
        assert_that(self.undertest.get_processed_indices(TEST_FILE_1),
                    contains_inanyorder(1, 3, 0))

        # Check other clients can read them
        manager2 = self.create_independent_manager()
        assert_that(manager2.get_processed_indices(TEST_FILE_1),
                    contains_inanyorder(1, 3, 0))

    def test_get_processed_indices_empty_should_not_cause_error(self):
        assert_that(self.undertest.get_processed_indices(TEST_FILE_1),
                    has_length(0))

    def test_get_processed_indices_after_done(self):
        filename = TEST_FILE_1
        self.undertest.lock(filename)
        self.undertest.record_index(filename, 0)
        self.undertest.record_index(filename, 1)
        self.undertest.record_index(filename, 2)
        self.undertest.unlock(filename)

        assert_that(self.undertest.get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))

        self.undertest.lock(filename)
        self.undertest.record_done(filename)
        self.undertest.unlock(filename)

        assert_that(self.undertest.get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))

        # Double check with a second manager
        manager2 = self.create_independent_manager()
        assert_that(manager2.get_processed_indices(filename),
                    contains_inanyorder(0, 1, 2))


class NoTrackingTest(AbstractVOSpaceProgressManagerTestCase, FileReadingTestCase):
    def _tracks_partial_progress(self):
        return False

    def test_record_index_does_nothing(self):
        self.undertest.lock(TEST_FILE_1)
        self.undertest.record_index(TEST_FILE_1, 1)
        self.undertest.record_index(TEST_FILE_1, 3)
        self.undertest.record_index(TEST_FILE_1, 0)
        self.undertest.unlock(TEST_FILE_1)

        assert_that(self.has_property(TEST_FILE_1, progress.PROCESSED_INDICES_PROPERTY),
                    equal_to(False))

    def test_get_processed_indices_empty(self):
        self.undertest.lock(TEST_FILE_1)
        self.undertest.record_index(TEST_FILE_1, 1)
        self.undertest.record_index(TEST_FILE_1, 3)
        self.undertest.record_index(TEST_FILE_1, 0)
        self.undertest.unlock(TEST_FILE_1)

        assert_that(self.undertest.get_processed_indices(TEST_FILE_1), has_length(0))


if __name__ == '__main__':
    unittest.main()

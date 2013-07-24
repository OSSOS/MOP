__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, has_length, contains
from mock import patch, call

from ossos.gui.sync import SynchronizationManager
from ossos.gui.context import VOSpaceWorkingContext

FILE1 = "/home/username/astrom/123.measure3.mpc"
FILE2 = "/home/username/astrom/456.measure3.mpc"
FILE3 = "/home/username/astrom/789.measure3.mpc"


class SynchronizationManagerTest(unittest.TestCase):
    def setUp(self):
        self.remote_directory = "vos:drusk/OSSOS/tests"
        self.remote_context = VOSpaceWorkingContext(self.remote_directory)
        self.sync_manager = SynchronizationManager(self.remote_context)

    def test_get_remote_uri(self):
        assert_that(self.sync_manager.get_remote_uri(FILE1),
                    equal_to("vos:drusk/OSSOS/tests/123.measure3.mpc"))

    @patch("threading.Thread")
    def test_added_file_synced_immediately_when_sync_enabled(self, ThreadMock):
        self.sync_manager.enable_sync()
        self.sync_manager.add_syncable_file(FILE1)

        ThreadMock.assert_called_once_with(
            target=self.sync_manager.do_synchronize,
            args=(FILE1, ))

        assert_that(self.sync_manager.syncable_files, has_length(0))

    @patch("threading.Thread")
    def test_add_files_not_synced_until_sync_enabled(self, ThreadMock):
        self.sync_manager.disable_sync()
        self.sync_manager.add_syncable_file(FILE1)
        self.sync_manager.add_syncable_file(FILE2)
        self.sync_manager.add_syncable_file(FILE3)

        assert_that(self.sync_manager.syncable_files,
                    contains(FILE1, FILE2, FILE3))
        assert_that(ThreadMock.call_count, equal_to(0))

        self.sync_manager.enable_sync()

        expected_calls = [
            call(target=self.sync_manager.do_synchronize, args=(FILE1, )),
            call(target=self.sync_manager.do_synchronize, args=(FILE2, )),
            call(target=self.sync_manager.do_synchronize, args=(FILE3, ))]

        assert_that(ThreadMock.call_args_list, contains(*expected_calls))

        assert_that(self.sync_manager.syncable_files, has_length(0))

    @patch("ossos.gui.sync.storage")
    def test_do_synchronize_copies_correct_paths(self, storage_mock):
        self.sync_manager.do_synchronize(FILE1)
        storage_mock.copy.assert_called_once_with(
            FILE1, "vos:drusk/OSSOS/tests/123.measure3.mpc")


if __name__ == '__main__':
    unittest.main()

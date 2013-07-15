__author__ = "David Rusk <drusk@uvic.ca>"

import os
import tempfile
import unittest

from hamcrest import assert_that, equal_to, contains

from tests.base_tests import FileReadingTestCase
from ossos import storage
from ossos.storage import SyncingVOFile, InvalidURIError
from ossos.gui.context import VOSpaceWorkingContext

BASE_TEST_DIR = "vos:drusk/OSSOS/tests/"
CURRENT_TEST_DIR = BASE_TEST_DIR + "storage_tests"

TEST_FILE_1 = "file1"


class SyncingVOFileTest(FileReadingTestCase):
    def setUp(self):
        self.context = VOSpaceWorkingContext(CURRENT_TEST_DIR)
        self.undertest = None

    def tearDown(self):
        if self.undertest is None:
            return

        os.remove(self.undertest.get_local_filename())

        self._clean_vospace()

    def _clean_vospace(self):
        for filename in self.context.listdir():
            storage.delete_uri(self.context.get_full_path(filename))

    def test_local_file_created(self):
        self.undertest = SyncingVOFile(self.context.get_full_path(TEST_FILE_1))

        local_file = self.undertest.get_local_filename()

        assert_that(os.path.exists(local_file))

        assert_that(os.path.dirname(local_file),
                    equal_to(tempfile.gettempdir()))

    def test_open_read_write(self):
        self.undertest = SyncingVOFile(self.context.get_full_path(TEST_FILE_1))

        # The file has been opened locally only since we have not yet synced.
        assert_that(self.context.listdir(), contains())

        # It should be empty
        assert_that(self.undertest.read(), equal_to(""))

        # Until we write something to it
        message = "hello"
        self.undertest.write(message)

        # We have not yet synced, nothing has been written to VOSpace
        assert_that(self.context.listdir(), contains())

        # but we can read from the local file
        assert_that(self.undertest.read(), equal_to(message))

        self.undertest.flush()

        # Now that we have flushed, the file should exist in VOSpace...
        assert_that(self.context.listdir(), contains(TEST_FILE_1))

        #  ... and we should check it actually has the data
        vofile = storage.vofile(self.context.get_full_path(TEST_FILE_1),
                                os.O_RDONLY)
        assert_that(vofile.read(), equal_to(message))

    def test_open_invalid_uri(self):
        # Must provide a vos: uri
        self.assertRaises(InvalidURIError, SyncingVOFile, "testfile")

    def test_open_file_with_content(self):
        # Set up a VOSpace file with some text in it
        vofile = storage.vofile(self.context.get_full_path(TEST_FILE_1),
                                os.O_WRONLY)
        message = "test"
        vofile.write(message)
        vofile.close()

        self.undertest = SyncingVOFile(self.context.get_full_path(TEST_FILE_1))

        # Make sure we can see the text that was in the VOSpace file already
        assert_that(self.undertest.read(), equal_to(message))

        message2 = "hello"
        self.undertest.write(message2)

        # Make sure the new content was appended to the original content,
        # not overwriting it.
        assert_that(self.undertest.read(), equal_to(message + message2))

        self.undertest.flush()

        # Make sure these changes are in VOSpace
        vofile = storage.vofile(self.context.get_full_path(TEST_FILE_1),
                                os.O_RDONLY)
        assert_that(vofile.read(), equal_to(message + message2))

    def test_repeatable_read(self):
        self.undertest = SyncingVOFile(self.context.get_full_path(TEST_FILE_1))

        # It should be empty
        assert_that(self.undertest.read(), equal_to(""))

        # Until we write something to it
        message = "hello"
        self.undertest.write(message)

        # Which we can read back...
        assert_that(self.undertest.read(), equal_to(message))

        # ... as many times as we like
        assert_that(self.undertest.read(), equal_to(message))

    def test_close_flushes(self):
        self.undertest = SyncingVOFile(self.context.get_full_path(TEST_FILE_1))

        # It should be empty
        assert_that(self.undertest.read(), equal_to(""))

        # Until we write something to it
        message = "hello"
        self.undertest.write(message)

        self.undertest.close()

        # Make sure these changes are in VOSpace
        vofile = storage.vofile(self.context.get_full_path(TEST_FILE_1),
                                os.O_RDONLY)
        assert_that(vofile.read(), equal_to(message))


if __name__ == '__main__':
    unittest.main()

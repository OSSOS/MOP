__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, contains, contains_inanyorder

from tests.base_tests import FileReadingTestCase
from ossos import storage
from ossos.gui.context import VOSpaceWorkingContext
from ossos.gui.persistence import VOSpaceProgressManager

# TODO: don't use my own VOSpace
# BASE_TEST_DIR = "vos:OSSOS/tests/"
BASE_TEST_DIR = "vos:drusk/OSSOS/tests/"
PROTOTYPE_FILE = "data/prototype"
PERSISTENCE_TEST_DIR = BASE_TEST_DIR + "persistence_tests"

TEST_FILE_1 = "1.cands.astrom"
TEST_FILE_2 = "2.reals.astrom"
TEST_FILE_3 = "3.cands.astrom"


class VOSpaceProgressManagerTest(FileReadingTestCase):
    def create_vofile(self, destination):
        # Just copy a prototype file until I figure out how to do this
        # properly
        storage.copy(self.get_abs_path(PROTOTYPE_FILE), destination)

    def setUp(self):
        self.context = VOSpaceWorkingContext(PERSISTENCE_TEST_DIR)
        self.undertest = VOSpaceProgressManager(self.context)

        self.create_vofile(self.context.get_full_path(TEST_FILE_1))
        self.create_vofile(self.context.get_full_path(TEST_FILE_2))
        self.create_vofile(self.context.get_full_path(TEST_FILE_3))

    def tearDown(self):
        for filename in self.context.listdir():
            storage.delete_uri(self.context.get_full_path(filename))

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


if __name__ == '__main__':
    unittest.main()

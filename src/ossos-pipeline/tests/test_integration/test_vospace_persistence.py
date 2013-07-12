__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from ossos import storage
from ossos.gui.context import VOSpaceWorkingContext

# TODO: don't use my own VOSpace
# BASE_TEST_DIR = "vos:OSSOS/tests/"
BASE_TEST_DIR = "vos:drusk/OSSOS/tests/"
PROTOTYPE_FILE = BASE_TEST_DIR + "prototype"
PERSISTENCE_TEST_DIR = BASE_TEST_DIR + "persistence_tests"

TEST_FILE_1 = "1.cands.astrom"
TEST_FILE_2 = "2.reals.astrom"
TEST_FILE_3 = "3.cands.astrom"


class VOSpaceProgressManagerTest(unittest.TestCase):
    def create_vofile(self, destination):
        # Just copy a prototype file until I figure out how to do this
        # properly
        storage.copy(PROTOTYPE_FILE, destination)

    def setUp(self):
        self.context = VOSpaceWorkingContext(PERSISTENCE_TEST_DIR)

        self.create_vofile(self.context.get_full_path(TEST_FILE_1))
        self.create_vofile(self.context.get_full_path(TEST_FILE_2))
        self.create_vofile(self.context.get_full_path(TEST_FILE_3))

    def tearDown(self):
        for filename in self.context.listdir():
            storage.delete_uri(self.context.get_full_path(filename))

    def test_is_done(self):
        pass

    def test_get_done(self):
        pass

    def test_record_done(self):
        pass


if __name__ == '__main__':
    unittest.main()

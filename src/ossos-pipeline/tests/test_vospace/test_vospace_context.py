__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, contains_inanyorder

from ossos.gui.context import VOSpaceWorkingContext

BASE_TEST_DIR = "vos:OSSOS/integration_tests/"
LISTING_TEST_DIR = BASE_TEST_DIR + "listing_test"


class VOSpaceWorkingContextTest(unittest.TestCase):
    def test_get_listing(self):
        context = VOSpaceWorkingContext(LISTING_TEST_DIR)

        assert_that(context.get_listing(".cands.astrom"),
                    contains_inanyorder("xxx1.cands.astrom", "xxx2.cands.astrom"))
        assert_that(context.get_listing(".reals.astrom"),
                    contains_inanyorder("xxx3.reals.astrom", "xxx4.reals.astrom"))

    def test_open(self):
        context = VOSpaceWorkingContext(BASE_TEST_DIR)

        filehandle = context.open("README.txt")
        file_contents = filehandle.read()
        filehandle.close()

        assert_that(file_contents,
                    equal_to("This directory contains files for OSSOS integration tests.\n"))

    def test_get_file_size(self):
        context = VOSpaceWorkingContext(BASE_TEST_DIR)

        assert_that(context.get_file_size("empty.txt"), equal_to(0))
        assert_that(context.get_file_size("README.txt"), equal_to(59))

    def test_exists(self):
        context = VOSpaceWorkingContext(BASE_TEST_DIR)

        assert_that(context.exists("README.txt"), equal_to(True))
        assert_that(context.exists("no_such_file.txt"), equal_to(False))


if __name__ == "__main__":
    unittest.main()

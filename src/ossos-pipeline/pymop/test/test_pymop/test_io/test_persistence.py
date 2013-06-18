__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import assert_that, contains_inanyorder, has_length

from test.base_tests import FileReadingTestCase
from pymop.io import persistence


class PersistenceTest(FileReadingTestCase):
    def test_load_progress(self):
        progress = persistence.load_progress(self.get_abs_path("data/testdir2"))
        assert_that(progress.get_processed_cands_files(),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(progress.get_processed_reals_files(),
                    contains_inanyorder("xxx3.reals.astrom"))

    def test_load_progress_no_logfile(self):
        working_directory = self.get_abs_path("data/testdir3")
        progress = persistence.load_progress(working_directory)
        assert_that(progress.get_processed_cands_files(), has_length(0))
        assert_that(progress.get_processed_reals_files(), has_length(0))

        os.remove(os.path.join(working_directory, persistence.LOGFILE))


if __name__ == '__main__':
    unittest.main()

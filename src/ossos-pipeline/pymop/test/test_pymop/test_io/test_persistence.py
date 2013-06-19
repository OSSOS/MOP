__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import assert_that, contains_inanyorder, has_length

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io import persistence
from pymop.io.astrom import AstromWorkload

WD_HAS_LOG = "data/persistence_has_log"
WD_NO_LOG = "data/persistence_no_log"


class PersistenceTest(FileReadingTestCase):
    def test_load_progress(self):
        progress = persistence.load_progress(self.get_abs_path(WD_HAS_LOG))
        assert_that(progress.get_processed_cands_files(),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(progress.get_processed_reals_files(),
                    contains_inanyorder("xxx3.reals.astrom"))

        assert_that(progress.get_processed(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(progress.get_processed(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))

    def test_load_progress_no_logfile(self):
        working_directory = self.get_abs_path(WD_NO_LOG)
        progress = persistence.load_progress(working_directory)
        assert_that(progress.get_processed_cands_files(), has_length(0))
        assert_that(progress.get_processed_reals_files(), has_length(0))

        os.remove(os.path.join(working_directory, persistence.LOGFILE))

    def test_astrom_workload_filtered_reals(self):
        working_directory = self.get_abs_path(WD_HAS_LOG)
        progress = persistence.load_progress(working_directory)
        workload = AstromWorkload(working_directory, progress, tasks.REALS_TASK)

        expected_filenames = ["xxx1.reals.astrom", "xxx2.reals.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))

    def test_astrom_workload_filtered_cands(self):
        working_directory = self.get_abs_path(WD_HAS_LOG)
        progress = persistence.load_progress(working_directory)
        workload = AstromWorkload(working_directory, progress, tasks.CANDS_TASK)

        expected_filenames = ["xxx2.cands.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))


if __name__ == '__main__':
    unittest.main()

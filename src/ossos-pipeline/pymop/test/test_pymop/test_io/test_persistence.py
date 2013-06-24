__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains_inanyorder, has_length, contains

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io.persistence import ProgressManager
from pymop.io.astrom import AstromWorkload

WD_HAS_PROGRESS = "data/persistence_has_progress"
WD_NO_LOG = "data/persistence_no_log"


class ProgressManagerLoadingTest(FileReadingTestCase):
    def test_load_progress(self):
        progress_manager = ProgressManager(self.get_abs_path(WD_HAS_PROGRESS))

        assert_that(progress_manager.get_done(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(progress_manager.get_done(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))

    def test_astrom_workload_filtered_reals(self):
        working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        progress_manager = ProgressManager(working_directory)
        workload = AstromWorkload(working_directory, progress_manager,
                                  tasks.REALS_TASK)

        expected_filenames = ["xxx1.reals.astrom", "xxx2.reals.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))

    def test_astrom_workload_filtered_cands(self):
        working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        progress_manager = ProgressManager(working_directory)
        workload = AstromWorkload(working_directory, progress_manager,
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

    def test_write_progress(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.progress_manager.record_done(processed1)

        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains(processed1))

        # Create a second persistence manager and make sure it sees the changes
        manager2 = ProgressManager(self.working_directory)
        assert_that(manager2.get_done(tasks.CANDS_TASK), has_length(0))
        assert_that(manager2.get_done(tasks.REALS_TASK), contains(processed1))

    def test_write_progress_two_simultaneous_managers(self):
        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.progress_manager.record_done(processed1)

        assert_that(self.progress_manager.get_done(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.progress_manager.get_done(tasks.REALS_TASK),
                    contains(processed1))

        # Create a second simultaneous manager
        manager2 = ProgressManager(self.working_directory)
        processed2 = "xxx3.reals.astrom"
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


if __name__ == '__main__':
    unittest.main()

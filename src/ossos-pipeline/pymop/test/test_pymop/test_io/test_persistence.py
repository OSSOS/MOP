__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import assert_that, contains_inanyorder, has_length, contains

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io import persistence
from pymop.io.persistence import PersistenceManager
from pymop.io.astrom import AstromWorkload

WD_HAS_LOG = "data/persistence_has_log"
WD_HAS_PROGRESS = "data/persistence_has_progress"
WD_NO_LOG = "data/persistence_no_log"


class PersistenceTest(FileReadingTestCase):
    def test_load_progress(self):
        progress = persistence.load_progress(self.get_abs_path(WD_HAS_LOG))

        assert_that(progress.get_processed(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(progress.get_processed(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))

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


class PersistenceManagerTest(FileReadingTestCase):
    def test_load_progress(self):
        persistence_manager = PersistenceManager(self.get_abs_path(WD_HAS_PROGRESS))

        assert_that(persistence_manager.get_processed(tasks.CANDS_TASK),
                    contains_inanyorder("xxx1.cands.astrom", "xxx3.cands.astrom"))
        assert_that(persistence_manager.get_processed(tasks.REALS_TASK),
                    contains_inanyorder("xxx3.reals.astrom"))

    def test_astrom_workload_filtered_reals(self):
        working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        persistence_manager = PersistenceManager(working_directory)
        workload = AstromWorkload(working_directory, persistence_manager,
                                  tasks.REALS_TASK)

        expected_filenames = ["xxx1.reals.astrom", "xxx2.reals.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))

    def test_astrom_workload_filtered_cands(self):
        working_directory = self.get_abs_path(WD_HAS_PROGRESS)
        persistence_manager = PersistenceManager(working_directory)
        workload = AstromWorkload(working_directory, persistence_manager,
                                  tasks.CANDS_TASK)

        expected_filenames = ["xxx2.cands.astrom"]
        actual_filenames = [filename for filename, astromdata in workload]

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))


class PersistenceStartsWithNoLogTest(FileReadingTestCase):
    def setUp(self):
        self.working_directory = self.get_abs_path(WD_NO_LOG)

    def tearDown(self):
        # Get rid of the log file so it doesn't interfere with other tests
        os.remove(os.path.join(self.working_directory, persistence.LOGFILE))

    def test_load_progress_no_logfile(self):
        progress = persistence.load_progress(self.working_directory)

        assert_that(progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(progress.get_processed(tasks.REALS_TASK), has_length(0))

    def test_write_progress_new_logfile(self):
        progress = persistence.load_progress(self.working_directory)

        assert_that(progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(progress.get_processed(tasks.REALS_TASK), has_length(0))

        processed1 = "xxx2.reals.astrom"
        progress.record_processed(processed1, tasks.REALS_TASK)

        assert_that(progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(progress.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        # Close the progress object and reload to make sure the changes made
        # it to disk
        progress.close()

        reopened_progress = persistence.load_progress(self.working_directory)
        assert_that(reopened_progress.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(reopened_progress.get_processed(tasks.REALS_TASK),
                    contains(processed1))

    def test_write_progress_flush_twice_new_logfile(self):
        progress = persistence.load_progress(self.working_directory)

        assert_that(progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(progress.get_processed(tasks.REALS_TASK), has_length(0))

        processed1 = "xxx2.reals.astrom"
        progress.record_processed(processed1, tasks.REALS_TASK)

        assert_that(progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(progress.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        # Close the progress object and reload to make sure the changes made
        # it to disk
        progress.close()

        reopened_progress = persistence.load_progress(self.working_directory)
        assert_that(reopened_progress.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(reopened_progress.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        processed2 = "xxx3.reals.astrom"
        reopened_progress.record_processed(processed2, tasks.REALS_TASK)

        assert_that(reopened_progress.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(reopened_progress.get_processed(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))

        # Close the progress object and reload to make sure the changes made
        # it to disk
        reopened_progress.close()

        final_progress = persistence.load_progress(self.working_directory)

        assert_that(final_progress.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(final_progress.get_processed(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))

    def test_flush_progress(self):
        progress = persistence.load_progress(self.working_directory)

        processed1 = "xxx2.reals.astrom"
        progress.record_processed(processed1, tasks.REALS_TASK)
        progress.flush()

        test_progress1 = persistence.load_progress(self.working_directory)
        assert_that(test_progress1.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(test_progress1.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        processed2 = "xxx3.reals.astrom"
        progress.record_processed(processed2, tasks.REALS_TASK)
        progress.flush()

        test_progress2 = persistence.load_progress(self.working_directory)
        assert_that(test_progress2.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(test_progress2.get_processed(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))


class PersistenceManagerFreshDirectoryTest(FileReadingTestCase):
    def setUp(self):
        self.working_directory = self.get_abs_path(WD_NO_LOG)
        self.persistence_manager = PersistenceManager(self.working_directory)

    def tearDown(self):
        # Get rid of generated files so we don't interfere with other tests
        self.persistence_manager.clean()

    def test_load_progress_no_logs(self):
        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    has_length(0))

    def test_write_progress(self):
        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.persistence_manager.record_processed(processed1)

        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        # Create a second persistence manager and make sure it sees the changes
        manager2 = PersistenceManager(self.working_directory)
        assert_that(manager2.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(manager2.get_processed(tasks.REALS_TASK), contains(processed1))

    def test_write_progress_two_simultaneous_managers(self):
        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    has_length(0))

        processed1 = "xxx2.reals.astrom"
        self.persistence_manager.record_processed(processed1)

        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK), has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    contains(processed1))

        # Create a second simultaneous manager
        manager2 = PersistenceManager(self.working_directory)
        processed2 = "xxx3.reals.astrom"
        manager2.record_processed(processed2)

        # Make sure second manager sees both entries
        assert_that(manager2.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(manager2.get_processed(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))

        # Make sure original manager sees both entries
        assert_that(self.persistence_manager.get_processed(tasks.CANDS_TASK),
                    has_length(0))
        assert_that(self.persistence_manager.get_processed(tasks.REALS_TASK),
                    contains_inanyorder(processed1, processed2))


if __name__ == '__main__':
    unittest.main()

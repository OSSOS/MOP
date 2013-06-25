__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from mock import Mock
from hamcrest import (assert_that, equal_to, contains_inanyorder, is_not,
                      has_length)

from test.base_tests import FileReadingTestCase
from pymop import tasks
from pymop.io.astrom import AstromWorkload
from pymop.io.persistence import ProgressManager
from pymop.gui.models import ProcessCandidatesModel, ProcessRealsModel
from pymop.io.imgaccess import AsynchronousImageDownloadManager

FRESH_TEST_DIR = "data/model_persistence_fresh"
TEST_FILES = ["xxx1.cands.astrom", "xxx2.cands.astrom", "xxx3.reals.astrom", "xxx4.reals.astrom"]


class ModelPersistenceTest(FileReadingTestCase):
    def setUp(self):
        self.working_dir = self.get_abs_path(FRESH_TEST_DIR)
        self.progress_manager = ProgressManager(self.working_dir)
        self.download_manager = Mock(spec=AsynchronousImageDownloadManager)
        # Create the required model in each test case
        # Create the workload for the required task in each test case

        self.concurrent_progress_manager = ProgressManager(self.working_dir)

    def tearDown(self):
        for filename in os.listdir(self.working_dir):
            if filename not in TEST_FILES:
                os.remove(os.path.join(self.working_dir, filename))

    def test_record_progress_cands_multiple_files(self):
        self.workload = AstromWorkload(self.working_dir,
                                       self.progress_manager, tasks.CANDS_TASK)
        self.model = ProcessCandidatesModel(self.workload, self.download_manager)

        first_file = self.model.get_current_filename()

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(True))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1, 2))

        self.model.next_item()
        second_file = self.model.get_current_filename()
        assert_that(second_file, is_not(equal_to(first_file)))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    has_length(0))

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0, 1))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(True))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0, 1, 2))

    def test_record_progress_reals_skipping_item(self):
        self.workload = AstromWorkload(self.working_dir,
                                       self.progress_manager, tasks.REALS_TASK)
        self.model = ProcessRealsModel(self.workload, self.download_manager)

        first_file = self.model.get_current_filename()

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        # Try skipping forward over an item and coming back to it
        self.model.next_item()
        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 2))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 2, 1))


if __name__ == '__main__':
    unittest.main()

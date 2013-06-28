__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

# TODO: upgrade
from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

from mock import Mock
from hamcrest import (assert_that, equal_to, contains_inanyorder, is_not,
                      has_length)

from test.base_tests import FileReadingTestCase, DirectoryCleaningTestCase
from pymop import tasks
from pymop.io.workload import DirectoryManager
from pymop.io.astrom import AstromWorkload
from pymop.io.persistence import ProgressManager
from pymop.gui import models
from pymop.gui.models import ProcessCandidatesModel, ProcessRealsModel
from pymop.io.imgaccess import AsynchronousImageDownloadManager

FRESH_TEST_DIR = "data/model_persistence_fresh"
TEST_FILES = ["xxx1.cands.astrom", "xxx2.cands.astrom", "xxx3.reals.astrom", "xxx4.reals.astrom"]


class ModelPersistenceTest(FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        pub.unsubAll()

        self.working_dir = self.get_abs_path(FRESH_TEST_DIR)
        directory_manager = DirectoryManager(self.working_dir)
        self.progress_manager = ProgressManager(directory_manager)
        self.download_manager = Mock(spec=AsynchronousImageDownloadManager)
        # Create the required model in each test case
        # Create the workload for the required task in each test case

        concurrent_directory_manager = DirectoryManager(self.working_dir)
        self.concurrent_progress_manager = ProgressManager(concurrent_directory_manager)

    def get_test_directory(self):
        return self.working_dir

    def get_test_files(self):
        return TEST_FILES

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

    def test_file_processed_event(self):
        self.workload = AstromWorkload(self.working_dir,
                                       self.progress_manager, tasks.REALS_TASK)
        self.model = ProcessRealsModel(self.workload, self.download_manager)

        observer = Mock()
        pub.subscribe(observer.on_file_processed, models.MSG_FILE_PROC)

        filename = self.workload.get_current_filename()
        accepts_before_next_file = 9

        while accepts_before_next_file > 1:
            self.model.accept_current_item()
            self.model.next_item()
            assert_that(observer.on_file_processed.call_count, equal_to(0))
            accepts_before_next_file -= 1

        self.model.accept_current_item()
        assert_that(observer.on_file_processed.call_count, equal_to(1))

        # Make sure it was triggered with the right data
        args = observer.on_file_processed.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(models.MSG_FILE_PROC))
        assert_that(msg.data, equal_to(filename))

    def test_unlock_on_exit(self):
        self.workload = AstromWorkload(self.working_dir,
                                       self.progress_manager, tasks.REALS_TASK)
        self.model = ProcessRealsModel(self.workload, self.download_manager)

        current_file = self.workload.get_current_filename()

        assert_that(self.progress_manager.owns_lock(current_file), equal_to(True))
        self.model.exit()
        assert_that(self.progress_manager.owns_lock(current_file), equal_to(False))


if __name__ == '__main__':
    unittest.main()

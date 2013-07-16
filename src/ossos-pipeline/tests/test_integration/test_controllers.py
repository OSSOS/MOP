__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, ANY
from hamcrest import assert_that, equal_to, is_not, same_instance, has_length

from tests.base_tests import FileReadingTestCase, WxWidgetTestCase, DirectoryCleaningTestCase
from ossos.daophot import TaskError
from ossos.gui import tasks
from ossos.gui.context import LocalDirectoryWorkingContext
from ossos.gui.persistence import LocalProgressManager
from ossos.gui.controllers import ProcessRealsController
from ossos.gui.models import UIModel
from ossos.gui.views import ApplicationView
from ossos.astrom import AstromParser
from ossos.gui.downloads import AsynchronousImageDownloadManager
from ossos.gui.workload import WorkUnitProvider, RealsWorkUnitBuilder

TEST_MINOR_PLANET_NUMBER = "mpn01"
TEST_PROVISIONAL_NAME = "DNZOH00"
TEST_DISCOVERY_AST = "*"
TEST_NOTE1 = "A"
TEST_NOTE2 = "B"
TEST_DATE = "2013 04 09.43325"
TEST_DEC = 31.2123
TEST_RA = 27.213
TEST_MAG = "123.5"
TEST_BAND = "A"
TEST_OBS_CODE = "523"
TEST_COMMENT = "Test comment"


class ProcessRealsControllerTest(WxWidgetTestCase, FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        WxWidgetTestCase.setUp(self)

        parser = AstromParser()
        directory_manager = LocalDirectoryWorkingContext(
            self.get_abs_path("data/controller_testdir"))
        progress_manager = LocalProgressManager(directory_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(tasks.REALS_TASK),
                                             directory_manager, progress_manager,
                                             RealsWorkUnitBuilder(
                                                 parser,
                                                 directory_manager,
                                                 progress_manager))

        download_manager = Mock(spec=AsynchronousImageDownloadManager)

        self.model = UIModel(workunit_provider, progress_manager, download_manager)
        self.model.start_work()

        self.name_generator = Mock()
        self.controller = ProcessRealsController(self.model, self.name_generator)

    def tearDown(self):
        WxWidgetTestCase.tearDown(self)
        DirectoryCleaningTestCase.tearDown(self)

    def get_directory_to_clean(self):
        return self.get_abs_path("data/controller_testdir")

    def get_files_to_keep(self):
        return ["1584431p15.measure3.reals.astrom", "1616681p10.measure3.reals.astrom"]

    def test_reject_disables_validation_controls(self):
        comment = "test"
        view = self.controller.get_view()

        assert_that(view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_do_reject(comment)

        # We have moved to the next item, so it should still be enabled
        assert_that(view.is_source_validation_enabled(), equal_to(True))

        # Loop back to that first item
        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(False))

        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(True))

    def test_reject_last_item_disables_validation_controls(self):
        comment = "test"
        view = self.controller.get_view()

        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_do_reject(comment)

        # We have moved to the next item (looped back to beginning), so it should still be enabled
        assert_that(view.is_source_validation_enabled(), equal_to(True))

        # Move forward to that last item again
        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(False))

        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_next_obs()
        assert_that(view.is_source_validation_enabled(), equal_to(True))

    def accept_source_reading(self):
        self.controller.on_do_accept(TEST_MINOR_PLANET_NUMBER,
                                     TEST_PROVISIONAL_NAME,
                                     TEST_DISCOVERY_AST,
                                     TEST_NOTE1,
                                     TEST_NOTE2,
                                     TEST_DATE,
                                     TEST_RA,
                                     TEST_DEC,
                                     TEST_MAG,
                                     TEST_BAND,
                                     TEST_OBS_CODE,
                                     TEST_COMMENT,
                                     False
        )

    def reject_source_reading(self):
        self.controller.on_do_reject(TEST_COMMENT)

    def test_accept_moves_to_first_observation_of_new_workunit(self):
        workunit1 = self.model.get_current_workunit()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.accept_source_reading()

        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

        self.accept_source_reading()

        workunit2 = self.model.get_current_workunit()

        assert_that(workunit2, is_not(same_instance(workunit1)))

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

    def read_written_results(self):
        writer = self.model.get_current_workunit().get_writer()
        filehandle = writer.filehandle
        filehandle.seek(0)
        return filehandle.read()

    def test_writer_flushing(self):
        def assert_sources_written_equals(num_sources):
            if num_sources == 0:
                assert_that(self.read_written_results(), equal_to(""))
            else:
                # Each source has 3 readings, and each reading has a comment
                # line and MPC line
                assert_that(self.read_written_results().rstrip().split("\n"),
                            has_length(6 * num_sources))

        # Should flush only after accepting all readings for a source
        assert_sources_written_equals(0)
        self.accept_source_reading()
        assert_sources_written_equals(0)
        self.accept_source_reading()
        assert_sources_written_equals(0)
        self.accept_source_reading()
        assert_sources_written_equals(1)

        self.reject_source_reading()
        assert_sources_written_equals(1)
        self.reject_source_reading()
        assert_sources_written_equals(1)
        self.reject_source_reading()
        assert_sources_written_equals(2)

    def test_phot_error_handling(self):
        view_mock = Mock(spec=ApplicationView)
        self.controller.view = view_mock

        error_message = "Photometry failed"
        self.model.get_current_source_observed_magnitude = Mock(
            side_effect=TaskError(error_message))
        self.model.get_current_band = Mock(return_value="B")

        self.controller.on_accept()

        view_mock.show_accept_source_dialog.assert_called_once_with(
            (ANY,
             ANY,
             ANY,
             ANY,
             ANY,
             "", # obs_mag
             "", # band
             ANY,
             ANY,
             ANY,
             ANY,
             error_message, # default_comment
             True)  # phot_failed
        )


if __name__ == '__main__':
    unittest.main()

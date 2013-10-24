__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, ANY, patch
from hamcrest import assert_that, equal_to, is_not, same_instance

from tests.base_tests import FileReadingTestCase, WxWidgetTestCase, DirectoryCleaningTestCase
from ossos.astrom import AstromParser
from ossos.daophot import TaskError
from ossos.downloads.cutouts.source import SourceCutout
from ossos.gui import tasks
from ossos.gui.context import LocalDirectoryWorkingContext
from ossos.gui.progress import LocalProgressManager
from ossos.gui.controllers import ProcessRealsController
from ossos.gui.models.imagemanager import ImageManager
from ossos.gui.models.validation import ValidationModel
from ossos.gui.views.appview import ApplicationView
from ossos import mpc
from ossos.naming import ProvisionalNameGenerator
from ossos.gui.models.workload import WorkUnitProvider, RealsWorkUnitBuilder

TEST_MINOR_PLANET_NUMBER = "mpn01"
TEST_PROVISIONAL_NAME = "DNZOH00"
TEST_DISCOVERY_AST = "*"
TEST_NOTE1 = "A  earlier approximate position inferior"
TEST_NOTE2 = "C   CCD"
TEST_DATE = "2013 04 09.43325"
TEST_DEC = 31.2123
TEST_RA = 27.213
TEST_MAG = "123.5"
TEST_MAG_ERR = "5"
TEST_BAND = "A"
TEST_OBS_CODE = "523"
TEST_COMMENT = "Test comment"


class ProcessRealsControllerTest(WxWidgetTestCase, FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        WxWidgetTestCase.setUp(self)

        parser = AstromParser()
        context = LocalDirectoryWorkingContext(
            self.get_abs_path("data/controller_testdir"))
        progress_manager = LocalProgressManager(context)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(tasks.REALS_TASK),
                                             context, progress_manager,
                                             RealsWorkUnitBuilder(
                                                 parser,
                                                 context,
                                                 context,
                                                 progress_manager))

        image_manager = Mock(spec=ImageManager)

        self.model = ValidationModel(workunit_provider, image_manager, None)
        self.model.start_work()

        self.model.get_writer = Mock(return_value=Mock(spec=mpc.MPCWriter))

        # We don't actually have any images loaded, so mock this out
        source_cutout = Mock(spec=SourceCutout)
        source_cutout.pixel_x = 11
        source_cutout.pixel_y = 50
        self.model.get_current_cutout = Mock(return_value=source_cutout)

        x_cen = 10
        y_cen = 50
        mag = 24.0
        self.model.get_current_source_observed_magnitude = Mock(return_value=(x_cen, y_cen, mag))

        self.model.is_current_source_adjusted = Mock(return_value=False)
        self.model.get_current_fits_header = Mock()

        self.name_generator = Mock(spec=ProvisionalNameGenerator)
        self.name_generator.generate_name.return_value = TEST_PROVISIONAL_NAME

        class TestFactory(object):
            def __init__(self, model, name_generator):
                self.model = model
                self.name_generator = name_generator

            def create_controller(self, view):
                return ProcessRealsController(self.model, view,
                                              self.name_generator)

        self.view = ApplicationView(
            TestFactory(self.model, self.name_generator))
        self.controller = self.view.controller
        self.controller.display_current_image = Mock()

    def tearDown(self):
        WxWidgetTestCase.tearDown(self)
        DirectoryCleaningTestCase.tearDown(self)

    def get_directory_to_clean(self):
        return self.get_abs_path("data/controller_testdir")

    def get_files_to_keep(self):
        return ["1584431p15.measure3.reals.astrom", "1616681p10.measure3.reals.astrom"]

    @patch("ossos.gui.controllers.mpc.Observation", spec=mpc.Observation)
    def test_reject_disables_validation_controls(self, mock_Observation):
        comment = "test"

        assert_that(self.view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_do_reject(comment)

        # We have moved to the next item, so it should still be enabled
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))

        # Loop back to that first item
        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(False))

        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))

    @patch("ossos.gui.controllers.mpc.Observation", spec=mpc.Observation)
    def test_reject_last_item_disables_validation_controls(self, mock_Observation):
        comment = "test"

        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_do_reject(comment)

        # We have moved to the next item (looped back to beginning), so it should still be enabled
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))

        # Move forward to that last item again
        self.controller.on_next_obs()
        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(False))

        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))
        self.controller.on_next_obs()
        assert_that(self.view.is_source_validation_enabled(), equal_to(True))

    def accept_source_reading(self):
        self.controller.on_do_accept(TEST_MINOR_PLANET_NUMBER,
                                     TEST_PROVISIONAL_NAME,
                                     TEST_NOTE1,
                                     TEST_NOTE2,
                                     TEST_DATE,
                                     TEST_RA,
                                     TEST_DEC,
                                     TEST_MAG,
                                     TEST_MAG_ERR,
                                     TEST_BAND,
                                     TEST_OBS_CODE,
                                     TEST_COMMENT
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

    def test_phot_error_handling(self):
        view_mock = Mock(spec=ApplicationView)
        self.controller.view = view_mock

        error_message = "Photometry failed"
        self.model.get_current_source_observed_magnitude = Mock(
            side_effect=TaskError(error_message))
        self.model.get_current_band = Mock(return_value="B")

        self.controller.on_accept()

        view_mock.show_accept_source_dialog.assert_called_once_with(
            ANY,
            ANY,
            ANY,
            ANY,
            "", # obs_mag
            -1, # obs_mag_err
            "", # band
            note1_choices=ANY,
            note2_choices=ANY,
            note1_default=ANY,
            note2_default=ANY,
            default_observatory_code=ANY,
            default_comment=error_message,
            phot_failure=True
        )


if __name__ == '__main__':
    unittest.main()

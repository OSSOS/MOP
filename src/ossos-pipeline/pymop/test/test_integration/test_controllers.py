__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock
from hamcrest import assert_that, equal_to

from test.base_tests import FileReadingTestCase, WxWidgetTestCase, DirectoryCleaningTestCase
from pymop.gui import tasks
from pymop.gui.app import DirectoryContext
from pymop.gui.persistence import ProgressManager
from pymop.gui.controllers import ProcessRealsController
from pymop.gui.models import UIModel
from pymop.tools.astrom import AstromParser
from pymop.gui.downloads import AsynchronousImageDownloadManager
from pymop.gui.workload import WorkUnitProvider, RealsWorkUnitBuilder


class ProcessRealsControllerTest(WxWidgetTestCase, FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        WxWidgetTestCase.setUp(self)

        parser = AstromParser()
        directory_manager = DirectoryContext(
            self.get_abs_path("data/controller_testdir"))
        progress_manager = ProgressManager(directory_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(tasks.REALS_TASK),
                                             directory_manager, progress_manager,
                                             RealsWorkUnitBuilder(parser, progress_manager))

        download_manager = Mock(spec=AsynchronousImageDownloadManager)

        self.model = UIModel(workunit_provider, progress_manager, download_manager)

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


if __name__ == '__main__':
    unittest.main()

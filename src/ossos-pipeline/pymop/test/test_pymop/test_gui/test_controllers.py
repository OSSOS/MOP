__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from mock import Mock
from hamcrest import assert_that, equal_to

from test.base_tests import FileReadingTestCase, WxWidgetTestCase
from pymop import tasks
from pymop.io.persistence import ProgressManager
from pymop.io.astrom import AstromWorkload
from pymop.gui.controllers import ProcessRealsController
from pymop.gui.models import ProcessRealsModel


class ProcessRealsControllerTest(WxWidgetTestCase, FileReadingTestCase):
    def setUp(self):
        super(ProcessRealsControllerTest, self).setUp()

        progress_manager = Mock(spec=ProgressManager)
        progress_manager.get_done.return_value = []

        workload = AstromWorkload(self.get_abs_path("data/controller_testdir"),
                                  progress_manager, tasks.REALS_TASK)
        download_manager = Mock()

        self.model = ProcessRealsModel(workload, download_manager)

        self.task = Mock()
        self.name_generator = Mock()
        self.controller = ProcessRealsController(self.task, self.model, self.name_generator)

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

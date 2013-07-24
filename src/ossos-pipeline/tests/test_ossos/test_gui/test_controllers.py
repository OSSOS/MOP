__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock, patch, ANY
from hamcrest import equal_to, assert_that

from ossos.gui import config
from ossos.gui.models import UIModel
from ossos.gui.controllers import AbstractController, ProcessRealsController
from ossos.naming import ProvisionalNameGenerator


class AbstractControllerTest(unittest.TestCase):
    def setUp(self):
        self.model = MagicMock(spec=UIModel)

        self.adjusted_x = 150
        self.adjusted_y = 250

        self.model.get_current_pixel_source_point.return_value = (self.adjusted_x, self.adjusted_y)

        self.original_x = 100
        self.original_y = 200

        def reset_location():
            self.model.get_current_pixel_source_point.return_value = (self.original_x, self.original_y)

        self.model.reset_current_source_location.side_effect = reset_location

        # TODO: indicates refactoring needed
        with patch("ossos.gui.controllers.ApplicationView"):
            self.controller = AbstractController(self.model)
            self.view = self.controller.get_view()

    def test_reset_source_location_updates_model(self):
        self.controller.on_reset_source_location()
        assert_that(self.model.reset_current_source_location.call_count,
                    equal_to(1))

    def test_reset_source_location_redraws_circle_at_original_location(self):
        self.controller.on_reset_source_location()
        self.view.draw_circle.assert_called_once_with(
            self.original_x, self.original_y, ANY, redraw=True)

        # Don't need to redraw whole image
        assert_that(self.view.display_current_image.call_count, equal_to(0))


class RealsControllerTest(unittest.TestCase):
    def setUp(self):
        self.model = MagicMock(spec=UIModel)

        # TODO: indicates refactoring needed
        with patch("ossos.gui.controllers.ApplicationView"):
            self.controller = ProcessRealsController(
                self.model, Mock(spec=ProvisionalNameGenerator))
            self.view = self.controller.get_view()

    def test_accept_sets_note1_to_hand_adjusted_if_current_source_adjusted(self):
        self.model.is_current_source_adjusted.return_value = True

        self.controller.on_accept()
        self.view.show_accept_source_dialog.assert_called_once_with(
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            note1_choices=ANY,
            note2_choices=ANY,
            note1_default=config.read("MPC.NOTE1_HAND_ADJUSTED"),
            note2_default=ANY,
            default_observatory_code=ANY,
            default_comment=ANY,
            phot_failure=ANY)

    def test_accept_doesnt_set_note1_to_hand_adjusted_if_current_source_not_adjusted(self):
        self.model.is_current_source_adjusted.return_value = False

        self.controller.on_accept()
        self.view.show_accept_source_dialog.assert_called_once_with(
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            ANY,
            note1_choices=ANY,
            note2_choices=ANY,
            note1_default=None,
            note2_default=ANY,
            default_observatory_code=ANY,
            default_comment=ANY,
            phot_failure=ANY)


if __name__ == '__main__':
    unittest.main()

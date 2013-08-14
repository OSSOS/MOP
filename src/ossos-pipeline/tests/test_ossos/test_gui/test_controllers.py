__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock, patch, ANY
from hamcrest import equal_to, assert_that

from ossos.gui import config
from ossos.astrom import SourceReading, Observation
from ossos.gui.views.app import ApplicationView
from ossos.gui.models import UIModel
from ossos.gui.controllers import AbstractController, ProcessRealsController, ImageLoadingDialogManager
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
            self.view = self.controller.view

    def test_reset_source_location_updates_model(self):
        self.controller.on_reset_source_location()
        assert_that(self.model.reset_current_source_location.call_count,
                    equal_to(1))

    def test_reset_source_location_redraws_circle_at_original_location(self):
        self.controller.on_reset_source_location()
        self.view.draw_marker.assert_called_once_with(
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
            self.view = self.controller.view

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

    def test_generate_mpc_line(self):
        obs = Observation("1234567", "p", "00")
        reading = SourceReading(334.56, 884.22, 335.56, 885.22, 0, 0,
                                335.56, 885.22, obs)
        self.model.get_current_reading = Mock(return_value=reading)

        assert_that(self.controller.generate_mpc_comment("Something fishy."),
                    equal_to("1234567p00 334.56 884.22 Something fishy.\n"))


class ImageLoadingDialogManagerTest(unittest.TestCase):
    def setUp(self):
        self.view = Mock(spec=ApplicationView)
        self.undertest = ImageLoadingDialogManager(self.view)

    def test_wait_for_source_shows_dialog(self):
        source_reading = Mock(spec=SourceReading)
        assert_that(self.view.show_image_loading_dialog.called, equal_to(False))
        self.undertest.wait_for_item(source_reading)
        self.view.show_image_loading_dialog.assert_called_once_with()

    def test_dialog_hidden_when_all_sources_ready(self):
        source_reading1 = Mock(spec=SourceReading)
        source_reading2 = Mock(spec=SourceReading)

        self.undertest.wait_for_item(source_reading1)
        self.undertest.wait_for_item(source_reading2)

        self.view.show_image_loading_dialog.assert_called_once_with()
        assert_that(self.view.hide_image_loading_dialog.called, equal_to(False))

        self.undertest.set_item_done(source_reading1)
        assert_that(self.view.hide_image_loading_dialog.called, equal_to(False))
        self.undertest.set_item_done(source_reading2)

        self.view.hide_image_loading_dialog.assert_called_once_with()

    def test_set_item_done_that_wasnt_in_wait_list_gets_ignored(self):
        source_reading1 = Mock(spec=SourceReading)
        source_reading2 = Mock(spec=SourceReading)

        self.undertest.wait_for_item(source_reading1)
        self.undertest.set_item_done(source_reading2)

        self.view.show_image_loading_dialog.assert_called_once_with()
        assert_that(self.view.hide_image_loading_dialog.called, equal_to(False))

    def test_wait_on_item_twice_only_requires_setting_done_once(self):
        source_reading1 = Mock(spec=SourceReading)
        self.undertest.wait_for_item(source_reading1)
        self.undertest.wait_for_item(source_reading1)

        self.view.show_image_loading_dialog.assert_called_once_with()

        self.undertest.set_item_done(source_reading1)
        self.view.hide_image_loading_dialog.assert_called_once_with()


if __name__ == '__main__':
    unittest.main()

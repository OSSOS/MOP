__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock, ANY
from hamcrest import equal_to, assert_that

from ossos.astrom import SourceReading
from ossos.downloads.cutouts.source import SourceCutout
from ossos.gui.autoplay import AutoplayManager
from ossos.gui import config
from ossos.gui.models.validation import ValidationModel
from ossos.gui.views.appview import ApplicationView
from ossos.gui.controllers import (AbstractController, ProcessRealsController,
                                   ProcessTracksController,
                                   ImageLoadingDialogManager)
from ossos.naming import ProvisionalNameGenerator


class AbstractControllerTest(unittest.TestCase):
    def setUp(self):
        self.model = MagicMock(spec=ValidationModel)

        self.adjusted_x = 150
        self.adjusted_y = 250

        self.model.get_current_pixel_source_point.return_value = (self.adjusted_x, self.adjusted_y)

        self.original_x = 100
        self.original_y = 200

        def reset_location():
            self.model.get_current_pixel_source_point.return_value = (self.original_x, self.original_y)

        self.model.reset_current_source_location.side_effect = reset_location

        self.view = Mock(spec=ApplicationView)
        self.controller = AbstractController(self.model, self.view)
        self.autoplay_manager = Mock(spec=AutoplayManager)
        self.controller.autoplay_manager = self.autoplay_manager

    def test_reset_source_location_updates_model(self):
        self.controller.on_reset_source_location()
        assert_that(self.model.reset_current_source_location.call_count,
                    equal_to(1))

    def test_reset_source_location_redraws_circle_at_original_location(self):
        self.controller.on_reset_source_location()
        self.view.refresh_markers.assert_called_once_with()

        # Don't need to redraw whole image
        assert_that(self.view.display.call_count, equal_to(0))

    def test_reset_colormap(self):
        self.controller.on_reset_colormap()
        self.view.reset_colormap.assert_called_once_with()

    def test_toggle_reticule(self):
        self.controller.on_toggle_reticule_key()
        self.view.toggle_reticule.assert_called_once_with()

    def test_enable_disable_autoplay(self):
        self.controller.on_enable_autoplay()
        self.autoplay_manager.start_autoplay.assert_called_once_with()

        self.controller.on_disable_autoplay()
        self.autoplay_manager.stop_autoplay.assert_called_once_with()

    def test_display_current_image(self):
        cutout = Mock(spec=SourceCutout)
        self.model.get_current_cutout.return_value = cutout

        self.controller.display_current_image()
        self.view.display.assert_called_once_with(cutout)
        self.view.update_displayed_data.assert_called_once_with(
            self.model.get_reading_data(), self.model.get_header_data_list()
        )

        self.model.acknowledge_image_displayed.assert_called_once_with()

    def test_stop_waiting_for_item_when_loaded(self):
        image_loading_dialog_manager = Mock(spec=ImageLoadingDialogManager)
        self.controller.image_loading_dialog_manager = image_loading_dialog_manager

        displayable_item = Mock()
        event = Mock()
        event.data = displayable_item

        self.controller.on_image_loaded(event)

        image_loading_dialog_manager.set_item_done.assert_called_once_with(
            displayable_item)

    def test_enable_disable_autosync(self):
        self.controller.on_enable_auto_sync()
        self.model.enable_synchronization.assert_called_once_with()

        self.controller.on_disable_auto_sync()
        self.model.disable_synchronization.assert_called_once_with()


class RealsControllerTest(unittest.TestCase):
    def setUp(self):
        self.model = MagicMock(spec=ValidationModel)
        self.model.get_current_source_observed_magnitude.return_value = 10, 20, 100, 1

        self.view = Mock(spec=ApplicationView)
        self.controller = ProcessRealsController(
            self.model, self.view, Mock(spec=ProvisionalNameGenerator))

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

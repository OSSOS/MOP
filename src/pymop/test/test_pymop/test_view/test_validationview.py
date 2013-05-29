__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from mock import Mock
from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from pymop.view.core.validationview import SourceValidationPanel


class TestSourceValidationPanel(WxWidgetTestCase):
    def setUp(self):
        # TODO: put this in super class?  Same thing being done in most
        # of the WxWidgetTestCases
        self.app = wx.App()
        self.rootframe = wx.Frame(None)

    def test_accept_button(self):
        validation_controller = Mock()
        validation_panel = SourceValidationPanel(self.rootframe, validation_controller)

        assert_that(validation_controller.on_accept.call_count, equal_to(0))

        accept_button = self.get_child_by_label(
            validation_panel, validation_panel.accept_label)
        self.fire_button_click_event(accept_button)

        assert_that(validation_controller.on_initiate_accept.call_count, equal_to(1))
        assert_that(validation_controller.on_reject.call_count, equal_to(0))

    def test_reject_button(self):
        validation_controller = Mock()
        validation_panel = SourceValidationPanel(self.rootframe, validation_controller)

        assert_that(validation_controller.on_reject.call_count, equal_to(0))

        reject_button = self.get_child_by_label(
            validation_panel, validation_panel.reject_label)
        self.fire_button_click_event(reject_button)

        assert_that(validation_controller.on_accept.call_count, equal_to(0))
        assert_that(validation_controller.on_reject.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

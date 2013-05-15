__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import wx

from mock import Mock
from hamcrest import assert_that, equal_to, not_none

from test.base_tests import WxWidgetTestCase
from mopgui.model.astrodata import AstroDataModel
from mopgui.view.navview import NavPanel
from mopgui.controller.appcontrol import NavigationController


class NavigationPanelTest(WxWidgetTestCase):
    def setUp(self):
        self.app = wx.PySimpleApp()
        self.rootframe = wx.Frame(None)

        self.model = Mock(spec=AstroDataModel)
        self.controller = NavigationController(self.model)
        self.undertest = NavPanel(self.rootframe, self.controller)

    def tearDown(self):
        self.rootframe.Destroy()

    def test_press_next_source_button(self):
        next_button = self.get_child_by_label(self.undertest, "next source")
        assert_that(next_button, not_none())

        self.fire_button_click_event(next_button)
        assert_that(self.model.next_source.call_count, equal_to(1))

    def test_press_next_source_button(self):
        prev_button = self.get_child_by_label(self.undertest, "previous source")
        assert_that(prev_button, not_none())

        self.fire_button_click_event(prev_button)
        assert_that(self.model.previous_source.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

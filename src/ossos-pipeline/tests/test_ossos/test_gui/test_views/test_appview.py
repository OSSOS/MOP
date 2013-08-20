__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx
from mock import Mock
from hamcrest import assert_that, equal_to

from tests.base_tests import WxWidgetTestCase
from ossos.gui.controllers import AbstractController
from ossos.gui.views.appview import ApplicationView


class ApplicationViewTest(WxWidgetTestCase):
    def setUp(self):
        super(ApplicationViewTest, self).setUp()

        self.output_writer = Mock()
        self.name_generator = Mock()

        class TestFactory(object):
            def create_controller(self, view):
                return Mock(spec=AbstractController)

        self.appview = ApplicationView(TestFactory())
        self.controller = self.appview.controller

    def tearDown(self):
        self.appview.close()

    def test_press_exit(self):
        menubar = self.appview.mainframe.GetMenuBar()
        filemenu = menubar.GetMenu(menubar.FindMenu("File"))
        exit_button = self.get_menuitem_by_label(filemenu, "Exit")

        # Fire menu selection event
        event = wx.CommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED,
                                exit_button.GetId())
        self.appview.mainframe.ProcessEvent(event)

        assert_that(self.controller.on_exit.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

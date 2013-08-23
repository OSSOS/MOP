__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx
from mock import Mock
from hamcrest import assert_that, equal_to

from tests.base_tests import WxWidgetTestCase
from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.gui.controllers import AbstractController
from ossos.gui.views.appview import ApplicationView


class ApplicationViewTest(WxWidgetTestCase):
    def setUp(self):
        super(ApplicationViewTest, self).setUp()

        class TestFactory(object):
            def create_controller(self, view):
                return Mock(spec=AbstractController)

        self.appview = ApplicationView(TestFactory())
        self.controller = self.appview.controller

    def tearDown(self):
        self.appview.close()

    def mock_image_viewer(self):
        image_viewer = Mock(spec=SingletViewer)
        self.appview.image_view_manager.image_viewer = image_viewer
        return image_viewer

    def test_press_exit(self):
        menubar = self.appview.mainframe.GetMenuBar()
        filemenu = menubar.GetMenu(menubar.FindMenu("File"))
        exit_button = self.get_menuitem_by_label(filemenu, "Exit")

        # Fire menu selection event
        event = wx.CommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED,
                                exit_button.GetId())
        self.appview.mainframe.ProcessEvent(event)

        assert_that(self.controller.on_exit.call_count, equal_to(1))

    def test_reset_colormap(self):
        image_viewer = self.mock_image_viewer()

        self.appview.reset_colormap()
        image_viewer.reset_colormap.assert_called_once_with()

    def test_toggle_reticule(self):
        image_viewer = self.mock_image_viewer()

        self.appview.toggle_reticule()
        image_viewer.toggle_reticule.assert_called_once_with()


if __name__ == '__main__':
    unittest.main()

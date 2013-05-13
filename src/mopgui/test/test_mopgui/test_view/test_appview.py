import unittest

import wx
from mock import Mock
from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from mopgui.controller.appcontrol import ApplicationController


class ApplicationViewTest(WxWidgetTestCase):
    def setUp(self):
        self.model = Mock()
        self.model.get_reading_data.return_value = (("TEST1", 1), ("TEST2", 2))
        self.model.get_header_data_list.return_value = [("Key1", "Val1"), ("Key2", "Val2")]
        self.model.get_current_image_source_point.return_value = (0, 0)
        self.model.get_current_image_FWHM.return_value = 3
        self.model.get_current_source_number.return_value = 0
        self.model.get_source_count.return_value = 2

        self.image_viewer = Mock()
        self.appcontroller = ApplicationController(self.model, self.image_viewer,
                                                   unittest=True)
        self.appview = self.appcontroller.get_view()
        self.mainframe = self.appview.mainframe
        self.mainframe_close = Mock()
        self.mainframe.Close = self.mainframe_close

    def tearDown(self):
        self.appview.close()
        self.mainframe.Destroy()

    def test_press_exit(self):
        menubar = self.mainframe.GetMenuBar()
        filemenu = menubar.GetMenu(menubar.FindMenu("File"))
        exit_button = self.get_menuitem_by_label(filemenu, "Exit")

        # Fire menu selection event
        event = wx.CommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED,
                                exit_button.GetId())
        self.mainframe.ProcessEvent(event)

        assert_that(self.image_viewer.close.call_count, equal_to(1))
        assert_that(self.mainframe_close.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from mopgui.view import wxutil
from mopgui.view.mainview import MainFrame


class ApplicationView(object):
    """
    Provides the view's external interface.
    """

    def __init__(self, model, appcontroller, validationcontroller, navcontroller):
        self.model = model

        self.appcontroller = appcontroller
        self.validationcontroller = validationcontroller
        self.navcontroller = navcontroller

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame(model, appcontroller, validationcontroller,
                                   navcontroller)

    def launch(self, debug_mode=False, unittest=False):
        wx.CallAfter(self.mainframe.show_image_loading_dialog)
        wx.CallAfter(self.model.start_loading_images)

        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        if not unittest:
            self.mainframe.Show()
            self.wx_app.MainLoop()

    def close(self):
        self.mainframe.Close()

    @wxutil.guithread
    def show_image_loading_dialog(self):
        self.mainframe.show_image_loading_dialog()

    @wxutil.guithread
    def hide_image_loading_dialog(self):
        self.mainframe.hide_image_loading_dialog()

    @wxutil.guithread
    def set_source_status(self, current_source, total_sources):
        self.mainframe.set_source_status(current_source, total_sources)

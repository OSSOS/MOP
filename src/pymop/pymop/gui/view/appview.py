from pymop.gui.controller import validationcontrol
from pymop.gui.view import wxutil
from pymop.gui.view.core import finishedview

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection
from wx.lib.pubsub import Publisher as pub

from pymop.gui.view.core.mainview import MainFrame
from pymop.gui.view.core.acceptsourceview import AcceptSourceDialog


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
        self.accept_source_dialog = None

        # TODO refactor
        pub.subscribe(self.show_accept_source_dialog, validationcontrol.MSG_INITIATE_ACCEPT)
        pub.subscribe(self.close_accept_source_dialog, validationcontrol.MSG_DO_ACCEPT)
        pub.subscribe(self.close_accept_source_dialog, validationcontrol.MSG_CANCEL_ACCEPT)

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

    @wxutil.guithread
    def set_observation_status(self, current_obs, total_obs):
        self.mainframe.set_observation_status(current_obs, total_obs)

    @wxutil.guithread
    def set_loading_status(self, loaded, total):
        self.mainframe.set_loading_status(loaded, total)

    def show_accept_source_dialog(self, event):
        self.accept_source_dialog = AcceptSourceDialog(
            self.mainframe, self.validationcontroller, *event.data)
        self.accept_source_dialog.ShowModal()

    def close_accept_source_dialog(self, event):
        if self.accept_source_dialog is not None:
            self.accept_source_dialog.Destroy()

    def all_processed_should_exit_prompt(self):
        return finishedview.should_exit_prompt(self.mainframe)

    def as_widget(self):
        return self.mainframe

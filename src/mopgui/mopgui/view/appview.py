"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx
import wx.lib.inspection


class ApplicationView(object):
    def __init__(self):
        self.wx_app = wx.App(False)
        self.mainframe = MainFrame()

    def launch(self, debug_mode=False):
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        self.mainframe.Show()
        self.wx_app.MainLoop()


class MainFrame(wx.Frame):
    def __init__(self):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

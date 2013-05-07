"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx


class ApplicationView(object):
    def __init__(self):
        self.app = wx.App(False)
        self.mainframe = MainFrame()

    def launch(self):
        self.mainframe.Show()
        self.app.MainLoop()


class MainFrame(wx.Frame):
    def __init__(self):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

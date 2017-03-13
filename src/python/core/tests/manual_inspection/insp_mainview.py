__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from tests.manual_inspection.context import testutil
from ossos.gui.views.mainframe import MainFrame


def main():
    app = wx.App()
    frame = MainFrame(testutil.Dummy("Controller"))
    frame.Show()
    wx.lib.inspection.InspectionTool().Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

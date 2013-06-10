__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from pymop.gui import launcher
from context import testutil


def main():
    app = wx.App()
    wx.lib.inspection.InspectionTool().Show()
    launcher.run_wizard(testutil.Dummy("App launcher"))
    app.MainLoop()


if __name__ == "__main__":
    main()

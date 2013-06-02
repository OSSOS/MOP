__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from pymop.gui.view.appview import AppStatusBar


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    statusbar = AppStatusBar(rootframe)
    rootframe.SetStatusBar(statusbar)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

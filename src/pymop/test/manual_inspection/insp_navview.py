__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from context import testutil
from pymop.view.core.navview import NavPanel


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    NavPanel(rootframe, testutil.Dummy("Navigation Controller"))

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from tests.manual_inspection.context import testutil
from ossos.gui.views.navigation import NavPanel


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    NavPanel(rootframe, testutil.Dummy("Controller"))

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

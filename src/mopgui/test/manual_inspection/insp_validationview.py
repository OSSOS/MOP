__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from context import testutil
from mopgui.view.validationview import SourceValidationPanel


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    undertest = SourceValidationPanel(rootframe, testutil.Dummy("Controller"))

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

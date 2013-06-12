__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from context import testutil
from pymop.gui.taskselect import WorkingDirectorySelector


def main():
    wxapp = wx.App()

    app = testutil.Dummy("PymopApplication")
    selector = WorkingDirectorySelector(app)
    selector.run()

    wxapp.MainLoop()


if __name__ == "__main__":
    main()

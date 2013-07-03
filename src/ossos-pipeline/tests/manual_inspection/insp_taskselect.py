__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from tests.manual_inspection.context import testutil
from ossos.gui.taskselect import TaskSetupManager


def main():
    wxapp = wx.App()

    app = testutil.Dummy("ValidationApplication")
    selector = TaskSetupManager(app)
    selector.run()

    wxapp.MainLoop()


if __name__ == "__main__":
    main()

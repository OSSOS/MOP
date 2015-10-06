__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from tests.manual_inspection.context import testutil
from ossos.gui.views.errorhandling import CertificateDialog


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    # TODO helper test frame/panel for these test dialogs
    # (use in dialogs.py as well)
    panel = wx.Panel(rootframe, wx.ID_ANY)

    def onclick_launch(event):
        CertificateDialog(panel, testutil.Dummy("Handler"),
                          "This is an error message!").Show()

    button = wx.Button(panel, id=wx.ID_ANY, label="Launch dialog")
    button.Bind(wx.EVT_BUTTON, onclick_launch)

    wx.lib.inspection.InspectionTool().Show()
    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

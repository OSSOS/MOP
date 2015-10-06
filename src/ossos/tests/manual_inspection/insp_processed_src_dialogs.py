__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from tests.manual_inspection.context import testutil
from ossos.gui.views.validation import AcceptSourceDialog, RejectSourceDialog


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    # TODO helper test frame/panel for these test dialogs
    # (use in dialogs.py as well)
    panel = wx.Panel(rootframe, wx.ID_ANY)

    def onclick_accept_src(event):
        controller = testutil.Dummy("Accept Source Controller")
        note1_choices = ["n1a", "n1b"]
        note2_choices = ["n2a", "n2b", "n2c"]
        AcceptSourceDialog(panel, controller, "provisional-name-1",
                           "2012 01 01",
                           27.213, 31.2123, 123.4, "A",
                           note1_choices=note1_choices,
                           note2_choices=note2_choices).Show()

    def onclick_reject_src(event):
        controller = testutil.Dummy("Reject Source Controller")
        RejectSourceDialog(panel, controller).Show()

    sizer = wx.BoxSizer(wx.HORIZONTAL)

    accept_src_button = wx.Button(panel, id=wx.ID_ANY, label="Accept source dialog")
    accept_src_button.Bind(wx.EVT_BUTTON, onclick_accept_src)

    reject_src_button = wx.Button(panel, id=wx.ID_ANY, label="Reject source dialog")
    reject_src_button.Bind(wx.EVT_BUTTON, onclick_reject_src)

    sizer.Add(accept_src_button)
    sizer.Add(reject_src_button)

    panel.SetSizer(sizer)

    wx.lib.inspection.InspectionTool().Show()
    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from context import testutil
from pymop.gui.view.core.acceptsourceview import AcceptSourceDialog


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    # TODO helper test frame/panel for these test dialogs
    # (use in dialogs.py as well)
    panel = wx.Panel(rootframe, wx.ID_ANY)

    def onclick(event):
        controller = testutil.Dummy("Accept Source Controller")
        note1_choices = ["n1a", "n1b"]
        note2_choices = ["n2a", "n2b", "n2c"]
        AcceptSourceDialog(panel, controller, "provisional-name-1",
                           "2012 01 01",
                           27.213, 31.2123, "A",
                           note1_choices=note1_choices,
                           note2_choices=note2_choices).ShowModal()

    button = wx.Button(panel, id=wx.ID_ANY, label="Press Me")
    button.Bind(wx.EVT_BUTTON, onclick)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

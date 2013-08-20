__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui.views.loading import WaitingGaugeDialog


def main():
    class TestFrame(wx.Frame):
        def __init__(self, *args, **kwargs):
            super(TestFrame, self).__init__(*args, **kwargs)

            panel = wx.Panel(self, wx.ID_ANY)

            button = wx.Button(panel, id=wx.ID_ANY, label="Press Me")
            button.Bind(wx.EVT_BUTTON, self.onclick)

        def onclick(self, event):
            self.dlg = WaitingGaugeDialog(self, "Image loading...")
            self.dlg.ShowModal()

    app = wx.App()
    frame = TestFrame(None)
    frame.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

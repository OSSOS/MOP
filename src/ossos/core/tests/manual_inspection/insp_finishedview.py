__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui.views.dialogs import should_exit_prompt


def main():
    class TestFrame(wx.Frame):
        def __init__(self, *args, **kwargs):
            super(TestFrame, self).__init__(*args, **kwargs)

            panel = wx.Panel(self, wx.ID_ANY)

            button = wx.Button(panel, id=wx.ID_ANY, label="Press Me")
            button.Bind(wx.EVT_BUTTON, self.onclick)

        def onclick(self, event):
            user_choice = should_exit_prompt(self)
            print("Should exit: %s" % user_choice)

    app = wx.App()
    frame = TestFrame(None)
    frame.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

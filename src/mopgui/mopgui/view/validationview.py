__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class SourceValidationPanel(wx.Panel):
    def __init__(self, parent, validation_controller,
                 accept_label="Accept", reject_label="Reject"):
        super(SourceValidationPanel, self).__init__(parent)

        self.validation_controller = validation_controller

        self.accept_label = accept_label
        self.reject_label = reject_label

        self._init_ui()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="Source Validation")

        self.accept_button = wx.Button(self, label=self.accept_label)
        self.reject_button = wx.Button(self, label=self.reject_label)

        self.accept_button.Bind(wx.EVT_BUTTON, self.validation_controller.on_accept)
        self.reject_button.Bind(wx.EVT_BUTTON, self.validation_controller.on_reject)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.accept_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.reject_button, proportion=0, flag=wx.ALL, border=button_border)

        sbox_sizer.Add(hsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)


if __name__ == "__main__":
    # Quick acceptance test to see how it looks
    from test.testutil import Dummy

    app = wx.App()
    rootframe = wx.Frame(None)

    undertest = SourceValidationPanel(rootframe, Dummy("Controller"))

    rootframe.Show()
    app.MainLoop()

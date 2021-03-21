__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class NavPanel(wx.Panel):
    NEXT_LABEL = "Next"
    PREV_LABEL = "Previous"

    def __init__(self, parent, controller):
        super(NavPanel, self).__init__(parent)

        self.controller = controller

        self._init_ui()
        self._bind_events()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="Source Observations")

        self.status_text = wx.StaticText(self, label="Loading...")
        self.next_button = wx.Button(self, wx.ID_FORWARD, label=self.NEXT_LABEL)
        self.prev_button = wx.Button(self, wx.ID_BACKWARD, label=self.PREV_LABEL)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.status_text, flag=wx.ALIGN_CENTER | wx.TOP, border=10)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.prev_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.next_button, proportion=0, flag=wx.ALL, border=button_border)

        vsizer.Add(hsizer)

        sbox_sizer.Add(vsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)

    def _bind_events(self):
        self.next_button.Bind(wx.EVT_BUTTON, self._on_next)
        self.prev_button.Bind(wx.EVT_BUTTON, self._on_prev)

    def _on_next(self, event):
        self.controller.on_next_obs()

    def _on_prev(self, event):
        self.controller.on_previous_obs()

    def set_status(self, current_obs, total_obs):
        if not 0 <= current_obs <= total_obs:
            raise ValueError("No observations available")
        self.status_text.SetLabel("%s of %s" % (current_obs, total_obs))

        # Re-center text
        self.Layout()

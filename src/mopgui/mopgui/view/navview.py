__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class NavPanel(wx.Panel):
    def __init__(self, parent, navcontroller,
                 next_source_label="Next Source",
                 prev_source_label="Previous Source"):
        super(NavPanel, self).__init__(parent)

        self.navcontroller = navcontroller

        self.next_source_label = next_source_label
        self.prev_source_label = prev_source_label

        self._init_ui()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="Navigation")

        self.next_src_button = wx.Button(self, wx.ID_FORWARD, label=self.next_source_label)
        self.prev_src_button = wx.Button(self, wx.ID_BACKWARD, label=self.prev_source_label)

        self.next_src_button.Bind(wx.EVT_BUTTON, self.navcontroller.on_next_source)
        self.prev_src_button.Bind(wx.EVT_BUTTON, self.navcontroller.on_previous_source)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.next_src_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.prev_src_button, proportion=0, flag=wx.ALL, border=button_border)

        sbox_sizer.Add(hsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)

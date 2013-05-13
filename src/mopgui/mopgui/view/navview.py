import wx


class NavPanel(wx.Panel):
    def __init__(self, parent, model):
        super(NavPanel, self).__init__(parent)

        self.model = model

        self._init_ui()

    def _init_ui(self):
        next_source_button = wx.Button(self, wx.ID_FORWARD,
                                       label="Next Source")
        next_source_button.Bind(wx.EVT_BUTTON, self.on_next_source)

        previous_source_button = wx.Button(self, wx.ID_BACKWARD,
                                           label="Previous Source")
        previous_source_button.Bind(wx.EVT_BUTTON, self.on_previous_source)

        source_button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        source_button_sizer.Add(previous_source_button)
        source_button_sizer.Add(next_source_button)

        navbox = wx.StaticBox(self, label="Navigation")

        # Layout
        bsizer = wx.StaticBoxSizer(navbox, wx.VERTICAL)

        bsizer.Add(source_button_sizer, 0, flag=wx.TOP | wx.LEFT, border=5)

        border = wx.BoxSizer()
        border.Add(bsizer, 1, wx.EXPAND | wx.ALL, border=5)
        self.SetSizer(border)

    def on_next_source(self, event):
        self.model.next_source()

    def on_previous_source(self, event):
        self.model.previous_source()
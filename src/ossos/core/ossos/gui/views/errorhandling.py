__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class CertificateDialog(wx.Dialog):
    def __init__(self, parent, handler, error_message):
        super(CertificateDialog, self).__init__(parent, title="Certificate Error")

        self.handler = handler
        self.error_message = error_message

        self._init_ui()
        self._do_layout()

    def _init_ui(self):
        self.header_text = wx.StaticText(self, label="An error has occured "
                                                     "which likely indicates "
                                                     "your CADC certificate "
                                                     "is invalid:")
        self.error_text = wx.StaticText(self, label=self.error_message)
        error_font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_ITALIC,
                             wx.FONTWEIGHT_NORMAL)
        self.error_text.SetFont(error_font)

        self.prompt_text = wx.StaticText(self, label="Enter your CADC "
                                                     "credentials to get a "
                                                     "new certificate:")

        self.username_label = wx.StaticText(self, label="CADC Username: ")
        self.username_field = wx.TextCtrl(self)

        self.password_label = wx.StaticText(self, label="Password: ")
        self.password_field = wx.TextCtrl(self, style=wx.TE_PASSWORD)

        self.accept_button = wx.Button(self, label="Get certificate")
        self.cancel_button = wx.Button(self, label="Cancel")
        self.accept_button.Bind(wx.EVT_BUTTON, self.on_accept)
        self.cancel_button.Bind(wx.EVT_BUTTON, self.on_cancel)

        self.username_field.SetFocus()
        self.accept_button.SetDefault()

    def _do_layout(self):
        vsizer = wx.BoxSizer(wx.VERTICAL)

        flag = wx.ALIGN_CENTER | wx.ALL
        border = 10

        vsizer.Add(self.header_text, flag=flag, border=border)
        vsizer.Add(self.error_text, flag=flag, border=border)

        line = wx.StaticLine(self, -1, size=(20, -1), style=wx.LI_HORIZONTAL)
        vsizer.Add(line, flag=wx.GROW | wx.RIGHT | wx.TOP, border=5)

        vsizer.Add(self.prompt_text, flag=flag, border=border)

        input_sizer = wx.FlexGridSizer(cols=2, hgap=5, vgap=border)
        input_sizer.Add(self.username_label)
        input_sizer.Add(self.username_field, proportion=1, flag=wx.EXPAND)
        input_sizer.Add(self.password_label, wx.EXPAND)
        input_sizer.Add(self.password_field, proportion=1, flag=wx.EXPAND)

        input_sizer.AddGrowableCol(1, proportion=1)

        vsizer.Add(input_sizer, flag=wx.EXPAND)

        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        button_sizer.Add(self.accept_button, flag=wx.RIGHT, border=5)
        button_sizer.Add(self.cancel_button, flag=wx.LEFT, border=5)

        vsizer.Add(button_sizer, flag=flag, border=border)

        padding_sizer = wx.BoxSizer(wx.HORIZONTAL)
        padding_sizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(padding_sizer)

    def on_cancel(self, event):
        self.Close()

    def on_accept(self, event):
        username = self.username_field.GetValue()
        password = self.password_field.GetValue()

        self.handler.refresh_certificate(username, password)
        self.Close()


class RetryDownloadDialog(wx.Dialog):
    def __init__(self, parent, handler, error_message):
        super(RetryDownloadDialog, self).__init__(parent, title="Download Error")

        self.handler = handler
        self.error_message = error_message

        self._init_ui()
        self._do_layout()

    def _init_ui(self):
        self.header_text = wx.StaticText(self, label="One or more downloads "
                                                     "failed:")
        self.error_text = wx.StaticText(self, label=self.error_message)
        error_font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_ITALIC,
                             wx.FONTWEIGHT_NORMAL)
        self.error_text.SetFont(error_font)

        self.retry_button = wx.Button(self, label="Retry")
        self.cancel_button = wx.Button(self, label="Cancel")
        self.retry_button.Bind(wx.EVT_BUTTON, self.on_accept)
        self.cancel_button.Bind(wx.EVT_BUTTON, self.on_cancel)

        self.retry_button.SetDefault()

    def _do_layout(self):
        vsizer = wx.BoxSizer(wx.VERTICAL)

        flag = wx.ALIGN_CENTER | wx.ALL
        border = 10

        vsizer.Add(self.header_text, flag=flag, border=border)
        vsizer.Add(self.error_text, flag=flag, border=border)

        line = wx.StaticLine(self, -1, size=(20, -1), style=wx.LI_HORIZONTAL)
        vsizer.Add(line, flag=wx.GROW | wx.RIGHT | wx.TOP, border=5)

        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        button_sizer.Add(self.retry_button, flag=wx.RIGHT, border=5)
        button_sizer.Add(self.cancel_button, flag=wx.LEFT, border=5)

        vsizer.Add(button_sizer, flag=flag, border=border)

        padding_sizer = wx.BoxSizer(wx.HORIZONTAL)
        padding_sizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(padding_sizer)

    def on_cancel(self, event):
        self.Close()

    def on_accept(self, event):
        self.handler.retry_downloads()
        self.Close()

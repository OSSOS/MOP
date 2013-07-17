__author__ = "David Rusk <drusk@uvic.ca>"

import errno
import os

import requests
import wx

from ossos.gui import logger


class DownloadErrorHandler(object):
    def __init__(self, app):
        self.app = app

        self._failed_downloads = []

    def handle_error(self, error, downloadable_item):
        """
        Checks what error occured and looks for an appropriate solution.

        Args:
          error: Exception
            The error that has occured.
          downloadable_item: ossos.downloads.DownloadableItem
            The item that was being downloaded when the error occurred.
        """
        if not hasattr(error, "errno"):
            logger.critical("Unresolvable download error: %s" % str(error))
            raise error

        if error.errno == errno.EACCES:
            self.handle_certificate_problem(str(error))
        elif error.errno == errno.ECONNREFUSED:
            self.handle_connection_refused(str(error), downloadable_item)
        else:
            logger.critical("Unresolvable download error: %s" % str(error))
            raise error

    def handle_certificate_problem(self, error_message):
        logger.warning("Problem with CADC certificate")

        self.app.get_model().stop_loading_images()

        view = self.app.get_view()
        view.hide_image_loading_dialog()
        view.show_certificate_dialog(self, error_message)

    def refresh_certificate(self, username, password):
        download_certificate(username, password)

        logger.info("Downloaded new CADC certificate")

        model = self.app.get_model()
        model.refresh_vos_client()

        self.app.get_view().show_image_loading_dialog()
        model.start_loading_images()

    def handle_connection_refused(self, error_message, downloadable_item):
        logger.warning("Connection refused when downloading")

        self._failed_downloads.append(downloadable_item)
        self.app.get_view().show_retry_download_dialog(self, error_message)

    def retry_downloads(self):
        model = self.app.get_model()
        for downloadable_item in self._failed_downloads:
            model.retry_download(downloadable_item)


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
        vsizer.Add(line, flag=wx.GROW | wx.ALIGN_CENTER_VERTICAL | wx.RIGHT | wx.TOP, border=5)

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
        self.Destroy()

    def on_accept(self, event):
        username = self.username_field.GetValue()
        password = self.password_field.GetValue()

        self.handler.refresh_certificate(username, password)
        self.Destroy()


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
        vsizer.Add(line, flag=wx.GROW | wx.ALIGN_CENTER_VERTICAL | wx.RIGHT | wx.TOP, border=5)

        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        button_sizer.Add(self.retry_button, flag=wx.RIGHT, border=5)
        button_sizer.Add(self.cancel_button, flag=wx.LEFT, border=5)

        vsizer.Add(button_sizer, flag=flag, border=border)

        padding_sizer = wx.BoxSizer(wx.HORIZONTAL)
        padding_sizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(padding_sizer)

    def on_cancel(self, event):
        self.Destroy()

    def on_accept(self, event):
        self.handler.retry_downloads()
        self.Destroy()


def download_certificate(username, password):
    url = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cred/proxyCert?daysValid=7"
    response = requests.get(url, auth=(username, password))

    certfile = os.path.join(os.getenv("HOME"), ".ssl/cadcproxy.pem")
    with open(certfile, "wb") as filehandle:
        filehandle.write(response.content)

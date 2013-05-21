__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class AppStatusBar(wx.StatusBar):
    LOADING_MSG = "Loading..."

    def __init__(self, parent):
        super(AppStatusBar, self).__init__(parent)

        self.SetFieldsCount(2)
        self.SetStatusText(self.LOADING_MSG, 0)
        self.SetStatusText(self.LOADING_MSG, 1)

    def set_source_status(self, current_source, total_sources):
        self.SetStatusText("Source %d of %d" % (current_source, total_sources), 0)

    def get_source_status(self):
        return self.GetStatusText(0)

    def set_loading_status(self, loaded, total):
        self.SetStatusText("Loaded %d of %d images" % (loaded, total), 1)

    def get_loading_status(self):
        return self.GetStatusText(1)

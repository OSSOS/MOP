__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from tests.manual_inspection.context import testutil
from ossos.gui.views.app import MainFrame


def main():
    class DummyModel(object):
        def get_reading_data(self):
            return [("X", 111), ("Y", 222)]

        def get_header_data_list(self):
            return [("FWHM", "3.00"), ("SNR", 10)]

    app = wx.App()
    frame = MainFrame(DummyModel(), testutil.Dummy("Controller"))
    frame.Show()
    wx.lib.inspection.InspectionTool().Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

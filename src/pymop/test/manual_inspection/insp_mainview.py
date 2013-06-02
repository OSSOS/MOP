__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from context import testutil
from pymop.gui.views import MainFrame


def main():
    class DummyModel(object):
        def get_reading_data(self):
            return [("X", 111), ("Y", 222)]

        def get_header_data_list(self):
            return [("FWHM", "3.00"), ("SNR", 10)]

    app = wx.App()
    frame = MainFrame(DummyModel(), testutil.Dummy("Controller"))
    frame.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

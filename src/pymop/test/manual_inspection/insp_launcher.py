__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from pymop.gui import launcher


def main():
    app = wx.App()
    launcher.run_wizard()
    app.MainLoop()


if __name__ == "__main__":
    main()

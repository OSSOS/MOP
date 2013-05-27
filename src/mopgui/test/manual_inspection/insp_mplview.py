__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from mopgui.view.image.mplview import MPLImageViewer


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    MPLImageViewer(rootframe)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

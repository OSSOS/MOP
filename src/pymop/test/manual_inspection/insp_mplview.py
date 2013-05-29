__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np
import wx

from mock import Mock

from pymop.gui.view.image.mplview import MPLImageViewer


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    viewer = MPLImageViewer(rootframe)
    hdu = Mock()
    hdu.data = np.random.uniform(size=(512, 512))
    hdulist = [hdu]
    viewer.view_image(hdulist)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

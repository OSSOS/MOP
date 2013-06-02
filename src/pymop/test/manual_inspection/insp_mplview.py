__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from astropy.io import fits

import context
from pymop.gui.imgviewer import MPLImageViewer


TEST_FILE = "cutout_1200_2400_1350_2300-1616681p.fits"


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    viewer = MPLImageViewer(rootframe)

    hdulist = fits.open(context.get_test_data_path(TEST_FILE))
    viewer.view_image(hdulist)

    viewer.draw_circle(50, 50, 10)
    viewer.draw_circle(20, 70, 5)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

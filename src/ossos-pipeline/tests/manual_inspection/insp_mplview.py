__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from mock import Mock
from astropy.io import fits

from tests.manual_inspection import context
from ossos.fitsviewer.singletviewer import SingletViewer


TEST_FILE = "cutout_1200_2400_1350_2300-1616681p.fits"


def main():
    app = wx.App()
    rootframe = wx.Frame(None)

    viewer = SingletViewer(rootframe)

    hdulist = fits.open(context.get_test_data_path(TEST_FILE))
    fits_image = Mock()
    fits_image.as_hdulist.return_value = hdulist

    viewer.display(fits_image)

    viewer.draw_marker(50, 50, 10)
    viewer.draw_marker(20, 70, 5)

    rootframe.Show()
    app.MainLoop()


if __name__ == "__main__":
    main()

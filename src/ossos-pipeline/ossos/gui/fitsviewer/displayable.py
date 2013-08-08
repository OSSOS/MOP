__author__ = "David Rusk <drusk@uvic.ca>"

import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

from ossos.gui.fitsviewer.colormap import GrayscaleColorMap
from ossos.gui.fitsviewer.singletviewer import zscale


class DisplayableImageSinglet(object):
    def __init__(self, hdulist):
        """
        Args:
          hdulist: astropy.io.fits.HDUList
            The FITS image to be displayed.
        """
        self.hdulist = hdulist
        self.colormap = GrayscaleColorMap()
        self.figure = plt.figure()

        self._built = False

    @property
    def image_data(self):
        return self.hdulist[0].data

    @property
    def image_width(self):
        return self.image_data.shape[0]

    @property
    def image_height(self):
        return self.image_data.shape[1]

    def render(self, canvas=None):
        if not self._built:
            self._build_figure()

        if canvas is None:
            plt.show()
        else:
            canvas.figure = self.figure

    def _build_figure(self):
        # limits specified as [left, bottom, width, height]
        # leave 2.5% border on left and right
        axes = plt.Axes(self.figure, [0.025, 0.0, 0.95, 1.0])

        # Make the axes fit the image tightly
        axes.set_xlim([0, self.image_width])
        axes.set_ylim([0, self.image_height])

        # Add 1 because FITS images start at pixel 1,1 while matplotlib
        # starts at 0,0
        extent = (1, self.image_width + 1, self.image_height + 1, 1)
        axes_image = plt.imshow(zscale(self.image_data),
                                extent=extent,
                                cmap=self.colormap.as_mpl_cmap())

        # Create axes for colorbar.  Make it tightly fit the image.
        divider = make_axes_locatable(axes)
        cax = divider.append_axes("bottom", size="5%", pad=0.05)
        self.figure.colorbar(axes_image, orientation="horizontal", cax=cax)

        self._built = True

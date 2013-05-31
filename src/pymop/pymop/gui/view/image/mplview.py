__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas


class MPLImageViewer(object):
    """
    Display images using matplotlib.
    """

    def __init__(self, parent):
        # Create the actual mpl figure we will draw on
        self.figure = plt.figure()

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

    def view_image(self, hdulist):
        plt.imshow(hdulist[0].data, cmap="gray")

    def draw_circle(self, x, y, radius):
        # TODO
        pass

    def close(self):
        pass


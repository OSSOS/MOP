__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas


class MPLImageViewer(object):
    """
    Display images using matplotlib.
    """

    def __init__(self, parent):
        # Create the actual mpl figure we will manipulate
        self.figure = Figure()

        self.axes = self.figure.add_subplot(111)
        x = np.arange(0, 6, .01)
        y = np.sin(x ** 2) * np.exp(-x)
        self.axes.plot(x, y)

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

    def view_image(self, hdulist):
        pass

    def draw_circle(self, x, y, radius):
        pass

    def close(self):
        pass


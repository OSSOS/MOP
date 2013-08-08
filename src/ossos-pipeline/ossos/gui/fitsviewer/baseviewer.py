__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas


class MPLFitsViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent):
        self.parent = parent

        # The initial matplotlib figure to draw on
        self.figure = plt.figure()

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

    def redraw(self):
        self.canvas.draw()

    def release_focus(self):
        self.parent.SetFocus()

    def as_widget(self):
        return self.canvas

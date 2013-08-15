__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas


class WxMPLFitsViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent):
        self.parent = parent

        # The initial matplotlib figure (may be swapped out later)
        self.figure = plt.figure()

        # Create the canvas on which the figure is rendered
        self.canvas = FigureCanvas(parent, wx.ID_ANY, self.figure)

    def redraw(self):
        self.canvas.draw()

    def release_focus(self):
        self.parent.SetFocus()

    def enable(self):
        self.canvas.Show(True)

    def disable(self):
        self.canvas.Show(False)

    def as_widget(self):
        return self.canvas

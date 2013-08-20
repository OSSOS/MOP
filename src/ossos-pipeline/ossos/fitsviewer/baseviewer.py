__author__ = "David Rusk <drusk@uvic.ca>"


class WxMPLFitsViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent, canvas):
        self.parent = parent
        self.canvas = canvas

    def redraw(self):
        self.canvas.draw()

    def release_focus(self):
        self.parent.SetFocus()

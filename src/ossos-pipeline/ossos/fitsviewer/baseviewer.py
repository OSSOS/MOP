__author__ = "David Rusk <drusk@uvic.ca>"


class WxMPLFitsViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent, canvas):
        self.parent = parent
        self.canvas = canvas

        self.current_displayable = None

    def release_focus(self):
        self.parent.SetFocus()

    def reset_colormap(self):
        if self.current_displayable is not None:
            self.current_displayable.reset_colormap()

    def toggle_reticule(self):
        self.current_displayable.toggle_reticule()

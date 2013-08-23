__author__ = "David Rusk <drusk@uvic.ca>"


class WxMPLFitsViewer(object):
    """
    Display FITS images using matplotlib.
    """

    def __init__(self, parent, canvas):
        self.parent = parent
        self.canvas = canvas

        self.current_cutout = None
        self.current_displayable = None
        self._displayables_by_cutout = {}

    def display(self, cutout, mark_source=True):
        if cutout in self._displayables_by_cutout:
            displayable = self._displayables_by_cutout[cutout]
        else:
            displayable = self._create_displayable(cutout)
            self._displayables_by_cutout[cutout] = displayable

        self._detach_handlers(self.current_displayable)

        self.current_cutout = cutout
        self.current_displayable = displayable

        self._attach_handlers(self.current_displayable)

        self._do_render(self.current_displayable)

        if mark_source:
            self.mark_sources(cutout)

    def draw_error_ellipse(self, x, y, a, b, pa):
        """
        Draws an ErrEllipse with the spcified dimensions.  Only one ErrEllipse can be drawn and
        only once (not movable).
        """
        self.current_displayable.place_error_ellipse(x, y, a, b, pa)

    def mark_sources(self, cutout):
        pass

    def refresh_markers(self):
        self.mark_sources(self.current_cutout)

    def release_focus(self):
        self.parent.SetFocus()

    def reset_colormap(self):
        if self.current_displayable is not None:
            self.current_displayable.reset_colormap()

    def toggle_reticule(self):
        self.current_displayable.toggle_reticule()

    def _attach_handlers(self, displayable):
        pass

    def _detach_handlers(self, displayable):
        pass

    def _create_displayable(self, cutout):
        raise NotImplementedError()

    def _do_render(self, displayable):
        displayable.render(self.canvas)

__author__ = "David Rusk <drusk@uvic.ca>"

from matplotlib.colors import LinearSegmentedColormap


class GrayscaleColorMap(object):
    """
    An image color map, which allows its contrast and bias to be controlled.

    Refer to http://matplotlib.org/api/colors_api.html,
    specifically class matplotlib.colors.LinearSegmentedColormap
    to better understand how this works.

    For an example, also see
    http://matplotlib.org/examples/pylab_examples/custom_cmap.html?highlight=codex%20colormap

    For explanation of contrast and bias as in ds9, see:
    http://chandra-ed.harvard.edu/learning_ds9_page2.html
    """

    def __init__(self):
        self.set_defaults()

    def _build_cdict(self):
        lower_segment_x = self._clip(self.x_offset + 0.5 - self.x_spread / 2)
        upper_segment_x = self._clip(self.x_offset + 0.5 + self.x_spread / 2)

        max_y = self._clip(0.5 + (self.y_spread / 2))
        min_y = self._clip(0.5 - (self.y_spread / 2))

        if lower_segment_x > upper_segment_x:
            # If they try to go past maximum contrast, flip the colorbar
            # ds9 style
            lower_segment_x, upper_segment_x = upper_segment_x, lower_segment_x
            min_y, max_y = max_y, min_y

        # NOTE: max_y used at lower bounds and min_y used at upper bounds in
        # order to invert the image's colourmap by default.
        self.min_bounds = (0.0, max_y, max_y)
        self.lower_segment_bounds = (lower_segment_x, max_y, max_y)
        self.upper_segment_bounds = (upper_segment_x, min_y, min_y)
        self.max_bounds = (1.0, min_y, min_y)

        self.cdict = {}
        for color in ["red", "green", "blue"]:
            self.cdict[color] = [self.min_bounds, self.lower_segment_bounds,
                                 self.upper_segment_bounds, self.max_bounds]

    def set_defaults(self):
        self._contrast = 0.5
        self._bias = 0.5

        self.x_spread = 1.0
        self.y_spread = 1.0
        self.x_offset = 0.0

        self._build_cdict()

    def set_bias(self, bias):
        """
        Adjusts the image bias.

        Bias determines where the color changes start.  At low bias, low
        intensities (i.e., low pixel values) will have non-zero color
        differences, while at high bias only high pixel values will have
        non-zero differences

        Args:
          bias: float
            A number between 0 and 1.  Note that upon initialization the
            colormap has a default bias of 0.5.

        Returns: void
        """
        self.x_offset += (bias - self._bias)
        self._bias = bias

        self._build_cdict()

    def update_bias(self, bias_diff):
        self.set_bias(self._bias + bias_diff)

    def set_contrast(self, contrast):
        """
        Adjusts the image contrast.

        Contrast refers to the rate of change of color with color level.
        At low contrast, color changes gradually over many intensity
        levels, while at high contrast it can change rapidly within a
        few levels

        Args:
          contrast: float
            A number between 0 and 1.  Note that upon initialization the
            colormap has a default contrast value of 0.5.

        Returns: void
        """
        self._contrast = contrast

        self.x_spread = 2 * (1.0 - contrast)
        self.y_spread = 2.0 - 2 * (1.0 - contrast)

        self._build_cdict()

    def update_contrast(self, contrast_diff):
        self.set_contrast(self._contrast + contrast_diff)

    def _clip(self, value):
        """Clip to range 0 to 1"""
        return clip(value, 0, 1)

    def as_mpl_cmap(self):
        return LinearSegmentedColormap("CustomGrayscale", self.cdict)


def clip(value, min_val, max_val):
    """
    Clip a value to a certain range.

    Args:
      value: number
        The value to be clipped.
      min_val: number
        The lowest allowable value.
      max_val: number
        The maximum allowable value.

    Returns:
      clipped: number
        The original value if it is between min_val and max_val.
        If it is less than min_val, return min_val.  If it is greater
        than max_val, return max_val.

    Examples;
      clip(0.1, 0, 1) -> 0.1
      clip(-0.1, 0, 1) -> 0
    """
    return max(min(value, max_val), min_val)

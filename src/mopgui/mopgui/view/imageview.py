"""
Handles displaying image data to the user.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import ds9


class DS9ImageViewer(object):
    def __init__(self):
        self.ds9_instance = None

    def _set_configs(self, *configs):
        for config in configs:
            self.ds9_instance.set(config)

    def view_image(self, hdulist):
        if self.ds9_instance is None:
            self.ds9_instance = ds9.ds9()

        self.ds9_instance.set_pyfits(hdulist)

        # Default configuration
        self._set_configs("zscale", "zoom to fit")

    def draw_circle(self, x, y, radius):
        self.ds9_instance.set(
            "regions",
            "image; circle %s %s %s" % (str(x), str(y), str(radius)))

    def close(self):
        if self.ds9_instance is not None:
            try:
                self.ds9_instance.set("exit")
            except ValueError as err:
                # The user already closed ds9
                pass

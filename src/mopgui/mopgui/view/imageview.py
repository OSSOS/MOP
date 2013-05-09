"""
Handles displaying image data to the user.
"""

import ds9


class DS9ImageViewer(object):
    def __init__(self):
        self.ds9_instance = None

    def view_image(self, hdulist):
        if self.ds9_instance is None:
            self.ds9_instance = ds9.ds9()

        self.ds9_instance.set_pyfits(hdulist)

        # Default configuration
        self._set_configs("zscale", "zoom to fit")

    def _set_configs(self, *configs):
        for config in configs:
            self.ds9_instance.set(config)

    def close(self):
        self.ds9_instance.set("exit")

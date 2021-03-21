from ossos.gui import config

__author__ = "David Rusk <drusk@uvic.ca>"

try:
   import pyds9 as ds9
except:
   import ds9
   ds9.DS9 = ds9.ds9

from ...fitsviewer.singletviewer import SingletViewer
from ...fitsviewer.tripletviewer import TripletViewer
from ...gui import logger


class ImageViewManager(object):
    def __init__(self, mainframe, zoom=1):
        # Note: the figure we pass in is just a temporary placeholder.
        # 'Displayable Items' provide their own figure which the canvas can
        # be made to use, but it also requires one on its creation.
        logger.debug("Building {}".format(self))
        self._ds9 = None
        self.zoom = zoom
        self._singlet_viewer = SingletViewer(mainframe.main_panel, display=self.ds9)
        self._triplet_viewer = TripletViewer(mainframe.main_panel, display=self.ds9)

        self._image_viewer = self._singlet_viewer

    def set_ds9(self, level="PREF"):
        """
        Set the default values on the ds9 display.
        """
        self.set_zoom()
        ds9_settings = config.read("DS9."+level)
        for key in list(ds9_settings.keys()):
            value = ds9_settings[key]
            cmd = key.replace("_", " ")
            self.ds9.set("{} {}".format(cmd, value))

    def set_zoom(self):
        cmd = "zoom {}".format(self.zoom)
        self.ds9.set(cmd)

    def _new_frame(self):
        self.ds9.set('frame new')

    @property
    def ds9(self):
        if not self._ds9:
            # start xpans if needed
            logger.debug("Starting XPANS")
            ds9.ds9_xpans()

            logger.debug("Starting DS9")
            # start ds9 if need, or connect to existing
            try:
                self._ds9 = ds9.DS9(target='gui', start=False)
            except ValueError as ve:
                cnt = 0
                while cnt < 10:
                    cnt += 1
                    try:
                        self._ds9 = ds9.DS9(target='gui', start=True, wait=30, verify=True)
                        self.set_ds9(level="INIT")
                        self.set_ds9(level="PREF")
                        break
                    except ValueError as ve:
                        logger.debug(f"DS9 not started yet: {ve}")
                        pass
            finally:
                self._ds9.set("frame delete all")
                logger.debug(f"DS9 Operational")
        self._ds9.reset_preferences = self.set_ds9
        self._ds9.new_frame = self._new_frame
        return self._ds9

    @property
    def image_viewer(self):
        """

        @rtype: SingletViewer
        """
        return self._image_viewer

    @image_viewer.setter
    def image_viewer(self, viewer):
        self._image_viewer = viewer

    def use_singlets(self):
        if self.image_viewer == self._triplet_viewer:
            self.image_viewer = self._singlet_viewer

    def use_triplets(self):
        if self.image_viewer == self._singlet_viewer:
            self.image_viewer = self._triplet_viewer

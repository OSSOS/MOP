__author__ = "David Rusk <drusk@uvic.ca>"

import logging

import ds9

from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.fitsviewer.tripletviewer import TripletViewer


class ImageViewManager(object):
    def __init__(self, mainframe):
        # Note: the figure we pass in is just a temporary placeholder.
        # 'Displayable Items' provide their own figure which the canvas can
        # be made to use, but it also requires one on its creation.

        self._ds9 = None
        self._singlet_viewer = SingletViewer(mainframe.main_panel, display=self.ds9)
        self._triplet_viewer = TripletViewer(mainframe.main_panel, display=self.ds9)

        self._image_viewer = self._singlet_viewer

    @property
    def ds9(self):
        if not self._ds9:
            # start xpans if needed
            ds9.ds9_xpans()

            # start ds9 if need, or connect to existing
            cnt = 0
            while cnt < 10:
                cnt += 1
                try:
                    self._ds9 = ds9.ds9(target='validate')
                    self._turn_off_view_panels()
                    self._ds9.set('width 640')
                    self._ds9.set('height 640')
                    self._ds9.set('scale mode zscale')
                    self._ds9.set('scale histeq')
                    self._ds9.set('cmap grey')
                    self._ds9.set('cmap invert yes')
                    self._ds9.set('zoom to 1')
                    self._ds9.set('frame delete all')
                    print "Done initializing ds9"
                    break
                except ValueError as ve:
                    logging.warning('Error on attempt {0} to connect to DS9 {1}'.format(cnt, ve))

        if self._ds9 is None:
            raise IOError("Failed to connect to DS9.")

        return self._ds9

    @property
    def image_viewer(self):
        """

        @rtype: SingletViewer
        """
        return self._image_viewer

    def _turn_off_view_panels(self):
        for panel in ['info', 'panner', 'buttons']:
            self._ds9.set('view {} no'.format(panel))
        self._ds9.set('view magnifier yes')

    @image_viewer.setter
    def image_viewer(self, viewer):
        self._image_viewer = viewer

    def use_singlets(self):
        if self.image_viewer == self._triplet_viewer:
            self.image_viewer = self._singlet_viewer

    def use_triplets(self):
        if self.image_viewer == self._singlet_viewer:
            self.image_viewer = self._triplet_viewer

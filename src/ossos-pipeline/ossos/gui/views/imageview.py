__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigureCanvas

from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.fitsviewer.tripletviewer import TripletViewer


class ImageViewManager(object):
    def __init__(self, mainframe):
        # Note: the figure we pass in is just a temporary placeholder.
        # 'Displayable Items' provide their own figure which the canvas can
        # be made to use, but it also requires one on its creation.
        #self.canvas = FigureCanvas(mainframe.main_panel, wx.ID_ANY, plt.figure())
        #mainframe.add_to_main_sizer(self.canvas)

        self._singlet_viewer = SingletViewer(mainframe.main_panel)
        self._triplet_viewer = TripletViewer(mainframe.main_panel)

        self._image_viewer = self._singlet_viewer

    @property
    def image_viewer(self):
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

__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.fitsviewer.tripletviewer import TripletViewer


class ImageViewManager(object):
    def __init__(self, mainframe):
        self.mainframe = mainframe

        self.singlet_viewer = SingletViewer(mainframe.main_panel)
        self.mainframe.add_to_main_sizer(self.singlet_viewer.canvas)

        self.triplet_viewer = None

        self._image_viewer = self.singlet_viewer

    @property
    def image_viewer(self):
        return self._image_viewer

    @image_viewer.setter
    def image_viewer(self, viewer):
        self._image_viewer = viewer

    def use_singlets(self):
        if self.image_viewer == self.triplet_viewer:
            self.triplet_viewer.disable()
            self.singlet_viewer.enable()
            self.image_viewer = self.singlet_viewer

    def use_triplets(self):
        if self.triplet_viewer is None:
            self.triplet_viewer = TripletViewer(self.mainframe.main_panel)
            self.mainframe.add_to_main_sizer(self.triplet_viewer.as_widget())

        if self.image_viewer == self.singlet_viewer:
            self.singlet_viewer.disable()
            self.triplet_viewer.enable()
            self.image_viewer = self.triplet_viewer

__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.fitsviewer.tripletviewer import TripletViewer


class ImageViewManager(object):
    def __init__(self, parent):
        self.parent = parent
        self.singlet_viewer = SingletViewer(parent)
        self.triplet_viewer = None

        self.image_viewer = self.singlet_viewer

    def use_singlets(self):
        if self.image_viewer == self.triplet_viewer:
            self.triplet_viewer.disable()
            self.singlet_viewer.enable()
            self.image_viewer = self.singlet_viewer

    def use_triplets(self):
        if self.triplet_viewer is None:
            self.triplet_viewer = TripletViewer(self.parent)

        if self.image_viewer == self.singlet_viewer:
            self.singlet_viewer.disable()
            self.triplet_viewer.enable()
            self.image_viewer = self.triplet_viewer

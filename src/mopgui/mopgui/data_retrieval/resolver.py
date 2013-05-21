"""
Resolves observation descriptions to their URIs.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from mopgui import config


class VOSpaceResolver(object):
    def __init__(self):
        self.dataset_root = config.read("IMG_RETRIEVAL.DATASET_ROOT")

    def resolve_uri(self, observation):
        # XXX can there be other file extensions?  For example, fits.fz?
        # Do we need to search the vospace directory and choose based on that?
        return "%s/%s/%s%s.fits" % (self.dataset_root, observation.expnum,
                                    observation.expnum, observation.ftype)

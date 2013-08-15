__author__ = "David Rusk <drusk@uvic.ca>"


class ImageNotLoadedException(Exception):
    """The requested image hasn't been loaded yet."""


class NoWorkUnitException(Exception):
    """No data is available at the current time."""

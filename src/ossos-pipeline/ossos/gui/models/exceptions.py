__author__ = "David Rusk <drusk@uvic.ca>"


class ImageNotLoadedException(Exception):
    """The requested image hasn't been loaded yet."""

    def __init__(self, requested_item):
        """
        Args:
          requested_item:
            The item whose image is not loaded yet.
        """
        self.requested_item = requested_item


class NoWorkUnitException(Exception):
    """No data is available at the current time."""


class NoAvailableWorkException(Exception):
    """"No more work is available."""


class SourceNotNamedException(Exception):
    """The source has no name."""

    def __init__(self, source):
        self.source = source

__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.downloads.data import SourceCutout
from ossos.downloads.focus import TripletFocalPointCalculator


class DownloadRequest(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self,
                 reading,
                 focal_point=None,
                 needs_apcor=False,
                 callback=None):
        """
        Constructor.

        Args:
          source_reading: ossos.astrom.SourceReading
            The reading which will be the focus of the downloaded image.
          focal_point: tuple(int, int)
            The x, y coordinates that should be the focus of the downloaded
            image.  These coordinates should be in terms of the
            source_reading parameter's coordinate system.
            Default value is None, in which case the source reading's x, y
            position is used as the focus.
          needs_apcor: bool
            If True, the apcor file with data needed for photometry
            calculations is downloaded in addition to the image.
            Defaults to False.
          callback: callable
            An optional callback to be called with the downloaded snapshot
            as its argument.
        """
        self.reading = reading
        self.needs_apcor = needs_apcor
        self.callback = callback

        if focal_point is None:
            self.focal_point = reading.source_point
        else:
            self.focal_point = focal_point


class TripletDownloadRequest(object):
    """
    Specifies a group of DownloadRequests which should be executed together.
    """

    def __init__(self, source, callback):
        self.source = source
        self.callback = callback

        self.downloads_finished = 0

        self._focal_point_calculator = TripletFocalPointCalculator()

    def notify_finished(self):
        self.downloads_finished += 1
        if self.downloads_finished == 9:
            self.callback(self.source)

    def iter_requests(self):
        for focal_point in self._focal_point_calculator.calculate_focal_points(
                self.source):
            yield DownloadRequest(focal_point.reading,
                                  focal_point=focal_point.point,
                                  needs_apcor=False,
                                  callback=self.notify_finished)

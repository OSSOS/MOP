__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.downloads.data import SourceSnapshot


class DownloadRequest(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self, downloader,
                 reading,
                 focal_point=None,
                 needs_apcor=False,
                 callback=None):
        """
        Constructor.

        Args:
          downloader:
            The downloader which will execute this request.
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
        self.downloader = downloader
        self.reading = reading
        self.needs_apcor = needs_apcor
        self.callback = callback

        if focal_point is None:
            self.focal_point = reading.source_point
        else:
            self.focal_point = focal_point

    def execute(self):
        hdulist, converter = self.downloader.download_fits(
            self.reading, self.focal_point)

        if self.needs_apcor:
            apcor = self.downloader.download_apcor(self.reading)
        else:
            apcor = None

        download = SourceSnapshot(self.reading, hdulist, converter, apcor)

        if self.callback is not None:
            self.callback(download)

        return download

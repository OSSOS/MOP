__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.download.data import DownloadedFitsImage


class DownloadRequest(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self, downloader, reading, focal_point, needs_apcor,
                 on_finished_callback):
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
          needs_apcor: bool
            If True, the apcor file with data needed for photometry
            calculations is downloaded in addition to the image.
        """
        self.downloader = downloader
        self.reading = reading
        self.focal_point = focal_point
        self.needs_apcor = needs_apcor
        self.on_finished_callback = on_finished_callback

    def execute(self):
        fits_str, converter = self.downloader.download_fits(
            self.reading, self.focal_point)

        if self.needs_apcor:
            apcor_str = self.downloader.download_apcor(self.reading)
        else:
            apcor_str = None

        download = DownloadedFitsImage(fits_str, converter, apcor_str)

        if self.on_finished_callback is not None:
            self.on_finished_callback(self.reading, download)

        return download
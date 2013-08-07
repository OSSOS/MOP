__author__ = "David Rusk <drusk@uvic.ca>"

import vos

from ossos.cutouts import CutoutCalculator
from ossos.download.downloads import DownloadedFitsImage
from ossos.gui import config
from ossos.gui import logger


class ImageSliceDownloader(object):
    """
    Downloads a slice of an image relevant to examining a (potential) source.
    """

    def __init__(self, slice_rows=None, slice_cols=None, vosclient=None):
        """
        Constructor.

        Args:
          resolver:
            Resolves source readings to the URI's from which they can be
            retrieved.
          slice_rows, slice_cols: int
            The number of rows and columns (pixels) to slice out around the
            source.  Leave as None to use default configuration values.
        """
        # If not provided, read defaults from application config file
        if slice_rows is None:
            slice_rows = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        if slice_cols is None:
            slice_cols = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")

        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        if vosclient is None:
            self.vosclient = vos.Client(cadc_short_cut=True)
        else:
            self.vosclient = vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def _download_fits_file(self, downloadable_item):
        image_uri = downloadable_item.get_image_uri()

        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            downloadable_item.get_extension(),
            downloadable_item.get_focal_point(),
            downloadable_item.get_full_image_size(),
            inverted=downloadable_item.is_inverted())

        logger.debug("Starting download: %s with cutout: %s"
                     % (image_uri, cutout_str))

        vofile = self.vosclient.open(image_uri, view="cutout",
                                     cutout=cutout_str)

        return vofile.read(), converter

    def _download_apcor_file(self, downloadable_item):
        apcor_uri = downloadable_item.get_apcor_uri()

        logger.debug("Starting download: %s" % apcor_uri)

        vofile = self.vosclient.open(apcor_uri, view="data")

        return vofile.read()

    def download(self, downloadable_item):
        """
        Retrieves a remote image.

        Args:
          downloadable_item: DownloadableItem
            Specification for the item to be downloaded.

        Returns:
          fitsimage: ossos.gui.image.DownloadedFitsImage
            The downloaded image, either in-memory or on disk as specified.
        """
        fits_str, converter = self._download_fits_file(downloadable_item)

        if downloadable_item.needs_apcor:
            apcor_str = self._download_apcor_file(downloadable_item)
        else:
            apcor_str = None

        return DownloadedFitsImage(fits_str, converter, apcor_str,
                                   in_memory=downloadable_item.in_memory)

    def refresh_vos_client(self):
        """
        If we have gotten a new certfile we have to create a new Client
        object before it will get used.
        """
        self.vosclient = vos.Client(cadc_short_cut=True)

__author__ = "David Rusk <drusk@uvic.ca>"

import threading

import vos

from ossos.gui import config
from ossos.gui.image import DownloadedFitsImage
from ossos.cutouts import CutoutCalculator


# Images from CCDs < 18 have their coordinates flipped
MAX_INVERTED_CCD = 17


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, downloader):
        self.downloader = downloader
        self.download_thread = None

    def start_download(self, workload,
                       image_loaded_callback=None,
                       all_loaded_callback=None):

        self.image_loaded_callback = image_loaded_callback
        self.all_loaded_callback = all_loaded_callback

        self.do_download(workload)

    def stop_download(self):
        assert self.download_thread is not None, "No download to stop."
        self.download_thread.stop()

    def do_download(self, workload):
        self.download_thread = SerialImageDownloadThread(
            self, self.downloader, workload)
        self.download_thread.start()

    def on_image_downloaded(self, downloaded_image, reading):
        if self.image_loaded_callback is not None:
            self.image_loaded_callback(reading, downloaded_image)

    def on_all_downloaded(self):
        if self.all_loaded_callback is not None:
            self.all_loaded_callback()


class SerialImageDownloadThread(threading.Thread):
    """
    Retrieve each image serially, but in this separate thread so it can
    happen in the background.
    """

    def __init__(self, loader, downloader, workload):
        super(SerialImageDownloadThread, self).__init__()

        self.download_manager = loader
        self.downloader = downloader
        self.workload = workload

        self._should_stop = False

    def run(self):
        for source in self.workload.get_sources():
            for reading in source.get_readings():
                if self._should_stop:
                    return

                downloaded_image = self.downloader.download(reading)

                if self._should_stop:
                    # Quit without calling callback
                    return

                self.download_manager.on_image_downloaded(downloaded_image,
                                                          reading)

        self.download_manager.on_all_downloaded()

    def stop(self):
        """Finish current download, but don't start any more."""
        self._should_stop = True


class VOSpaceResolver(object):
    """
    Resolves resources in VOSpace.
    """

    def __init__(self):
        self.dataset_root = config.read("IMG_RETRIEVAL.DATASET_ROOT")

    def resolve_image_uri(self, observation):
        # XXX can there be other file extensions?  For example, fits.fz?
        # Do we need to search the vospace directory and choose based on that?
        return "%s/%s/%s%s.fits" % (self.dataset_root, observation.expnum,
                                    observation.expnum, observation.ftype)

    def resolve_apcor_uri(self, observation):
        return "%s/%s/ccd%s/%s.apcor" % (self.dataset_root, observation.expnum,
                                         observation.ccdnum, observation.rawname)


class ImageSliceDownloader(object):
    """
    Downloads a slice of an image relevant to examining a (potential) source.
    """

    def __init__(self, resolver, slice_rows=None, slice_cols=None, vosclient=None):
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
        self.resolver = resolver

        # If not provided, read defaults from application config file
        if slice_rows is None:
            slice_rows = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_ROWS")
        if slice_cols is None:
            slice_cols = config.read("IMG_RETRIEVAL.DEFAULT_SLICE_COLS")

        self.slice_rows = slice_rows
        self.slice_cols = slice_cols

        self.vosclient = vos.Client() if vosclient is None else vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def _download_fits_file(self, uri, source_reading):
        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        ccdnum = int(source_reading.obs.ccdnum)
        extension = str(ccdnum + 1)

        imgsize = source_reading.get_original_image_size()

        inverted = True if ccdnum <= MAX_INVERTED_CCD else False

        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            extension, source_reading.reference_source_point, imgsize,
            inverted=inverted)

        vofile = self.vosclient.open(uri, view="cutout", cutout=cutout_str)

        return vofile.read(), converter

    def _download_apcor_file(self, uri):
        vofile = self.vosclient.open(uri, view="data")
        return vofile.read()

    def download(self, source_reading, in_memory=True):
        """
        Retrieves a remote image.

        Args:
          source_reading: pymop.io.parser.SourceReading
            The reading to take a cutout around.
          in_memory: bool
            If True, the image is stored in memory without being written to
            disk.  If False, the image will be written to a temporary file.

        Returns:
          fitsimage: pymop.io.img.FitsImage
            The downloaded image, either in-memory or on disk as specified.
        """
        image_uri = self.resolver.resolve_image_uri(source_reading.obs)
        apcor_uri = self.resolver.resolve_apcor_uri(source_reading.obs)

        fits_str, converter = self._download_fits_file(image_uri, source_reading)
        apcor_str = self._download_apcor_file(apcor_uri)

        return DownloadedFitsImage(fits_str, apcor_str, converter,
                                   in_memory=in_memory)

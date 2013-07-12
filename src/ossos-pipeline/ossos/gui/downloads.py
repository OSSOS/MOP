__author__ = "David Rusk <drusk@uvic.ca>"

import math
import Queue
import threading

import vos

from ossos.gui import config
from ossos.gui.image import DownloadedFitsImage
from ossos.cutouts import CutoutCalculator


# Images from CCDs < 18 have their coordinates flipped
MAX_INVERTED_CCD = 17

MAX_THREADS = 3


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, downloader, error_handler):
        """
        Constructor.

        Args:
          downloader:
            Downloads images.
          error_handler:
            Handles errors that occur when trying to download resources.
        """
        self.downloader = downloader
        self.error_handler = error_handler

        self._work_queue = Queue.Queue()

        self._workers = []
        self._maximize_workers()

    def start_downloading_workunit(self, workunit, image_loaded_callback=None):
        self._maximize_workers()

        # Load up queue with downloadable items
        needs_apcor = workunit.is_apcor_needed()
        for source in workunit.get_unprocessed_sources():
            for reading in source.get_readings():
                self._work_queue.put(
                    DownloadableItem(reading, source, needs_apcor,
                                     image_loaded_callback))

    def retry_download(self, downloadable_item):
        self._work_queue.put(downloadable_item)

    def stop_download(self):
        for worker in self._workers:
            worker.stop()

    def wait_for_downloads_to_stop(self):
        while not self._all_workers_stopped():
            pass

    def refresh_vos_client(self):
        self.downloader.refresh_vos_client()

    def _maximize_workers(self):
        self._prune_dead_workers()

        while len(self._workers) < MAX_THREADS:
            worker = DownloadThread(self._work_queue, self.downloader,
                                    self.error_handler)
            worker.daemon = True  # Thread quits when application does
            self._workers.append(worker)
            worker.start()

    def _prune_dead_workers(self):
        self._workers = filter(lambda thread: thread.is_alive(), self._workers)

    def _all_workers_stopped(self):
        for worker in self._workers:
            if not worker.is_stopped():
                return False

        return True


class DownloadThread(threading.Thread):
    def __init__(self, work_queue, downloader, error_handler):
        super(DownloadThread, self).__init__()

        self.work_queue = work_queue
        self.downloader = downloader
        self.error_handler = error_handler

        self._should_stop = False
        self._idle = True

    def run(self):
        while not self._should_stop:
            downloadable_item = self.work_queue.get()
            self._idle = False

            try:
                self.do_download(downloadable_item)
            except Exception as error:
                self.error_handler.handle_error(error, downloadable_item)
            finally:
                # It is up to the error handler to requeue the downloadable
                # item if needed.
                self.work_queue.task_done()
                self._idle = True

    def do_download(self, downloadable_item):
        downloaded_item = self.downloader.download(downloadable_item)

        if self._should_stop:
            return

        downloadable_item.finished_download(downloaded_item)

    def stop(self):
        self._should_stop = True

    def is_stopped(self):
        return self._should_stop and self._idle


class DownloadableItem(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self, reading, source, needs_apcor, on_finished_callback,
                 in_memory=True):
        """
        Constructor.

        Args:
          source_reading: ossos.astrom.SourceReading
            The reading which will be the focus of the downloaded image.
          source: ossos.astrom.Source
            The source for which the reading was taken.
          needs_apcor: bool
            If True, the apcor file with data needed for photometry
            calculations is downloaded in addition to the image.
          in_memory: bool
            If True, the image is stored in memory without being written to
            disk.  If False, the image will be written to a temporary file.
        """
        self.reading = reading
        self.source = source
        self.needs_apcor = needs_apcor
        self.on_finished_callback = on_finished_callback
        self.in_memory = in_memory

    def get_image_uri(self):
        return self.reading.get_image_uri()

    def get_apcor_uri(self):
        return self.reading.get_apcor_uri()

    def get_focal_point(self):
        """
        Determines what the focal point of the downloaded image should be.

        Returns:
          focal_point: (x, y)
            The location of the source in the middle observation, in the
            coordinate system of the current source reading.
        """
        middle_index = int(math.ceil((len(self.source.get_readings()) / 2)))
        middle_reading = self.source.get_reading(middle_index)

        offset_x, offset_y = self.reading.get_coordinate_offset(middle_reading)

        return middle_reading.x + offset_x, middle_reading.y + offset_y

    def get_full_image_size(self):
        """
        Returns:
          tuple(int width, int height)
            The full pixel size of the image before any cutouts.
        """
        return self.reading.get_original_image_size()

    def get_extension(self):
        """
        Returns:
          extension: str
            The FITS file extension to be downloaded.
        """
        if self._is_observation_fake():
            # We get the image from the CCD directory and it is not
            # multi-extension.
            return 0

        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        return str(self.get_ccd_num() + 1)

    def is_inverted(self):
        """
        Returns:
          inverted: bool
            True if the stored image is inverted.
        """
        if self._is_observation_fake():
            # We get the image from the CCD directory and it has already
            # been corrected for inversion.
            return False

        return True if self.get_ccd_num() <= MAX_INVERTED_CCD else False

    def get_ccd_num(self):
        """
        Returns:
          ccdnum: int
            The number of the CCD that the image is on.
        """
        return int(self.reading.get_observation().ccdnum)

    def finished_download(self, downloaded_item):
        """
        Triggers callbacks indicating the item has been downloaded.
        """
        self.on_finished_callback(self.reading, downloaded_item)

    def _is_observation_fake(self):
        return self.reading.get_observation().is_fake()


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
        cutout_str, converter = self.cutout_calculator.build_cutout_str(
            downloadable_item.get_extension(),
            downloadable_item.get_focal_point(),
            downloadable_item.get_full_image_size(),
            inverted=downloadable_item.is_inverted())

        vofile = self.vosclient.open(downloadable_item.get_image_uri(),
                                     view="cutout", cutout=cutout_str)

        return vofile.read(), converter

    def _download_apcor_file(self, downloadable_item):
        vofile = self.vosclient.open(downloadable_item.get_apcor_uri(),
                                     view="data")
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

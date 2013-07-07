__author__ = "David Rusk <drusk@uvic.ca>"

import threading

import vos

from ossos.gui import config
from ossos.gui.image import DownloadedFitsImage
from ossos.cutouts import CutoutCalculator


# Images from CCDs < 18 have their coordinates flipped
MAX_INVERTED_CCD = 17

MAX_THREADS = 6


class AsynchronousImageDownloadManager(object):
    """
    Coordinates the downloading of images asynchronously from the rest of
    the application.
    """

    def __init__(self, downloader):
        self.downloader = downloader

        self._should_stop = False

    def start_download(self, workload,
                       image_loaded_callback=None,
                       all_loaded_callback=None):
        """
        Creates a thread to download the specified workload.  That thread
        will in turn spawn other threads to download the images, using up
        to MAX_THREADS concurrently.

        The outer thread spawning the downloader threads is needed so that
        the application can still respond to callbacks and update the
        model as each downloader thread finishes.
        """
        args = (workload, image_loaded_callback, all_loaded_callback)
        workload_thread = threading.Thread(target=self._download_workload,
                                           args=args)
        workload_thread.start()

    def _download_workload(self, workload,
                           image_loaded_callback,
                           all_loaded_callback):
        items_to_download = []
        for source in workload.get_unprocessed_sources():
            for reading in source.get_readings():
                items_to_download.append(reading)

        index = 0
        threads = []

        while index < len(items_to_download) and not self._should_stop:
            threads = filter(lambda thread: thread.is_alive(), threads)

            if len(threads) < MAX_THREADS:
                args = (items_to_download[index], image_loaded_callback)
                new_thread = threading.Thread(target=self._do_download, args=args)
                new_thread.start()

                threads.append(new_thread)
                index += 1

        if not self.should_stop() and all_loaded_callback is not None:
            all_loaded_callback()

    def _do_download(self, item, on_finished):
        on_finished(item, self.downloader.download(item))

    def stop_download(self):
        self._should_stop = True

    def should_stop(self):
        return self._should_stop


class VOSpaceResolver(object):
    """
    Resolves resources in VOSpace.
    """

    def __init__(self):
        self.dataset_root = config.read("IMG_RETRIEVAL.DATASET_ROOT")

    def resolve_image_uri(self, observation):
        # TODO: make more general - have logic for trying alternative locations
        uri = "%s/%s/" % (self.dataset_root, observation.expnum)

        if observation.is_fake():
            uri += "ccd%s/%s.fits" % (observation.ccdnum, observation.rawname)
        else:
            uri += "%s%s.fits" % (observation.expnum, observation.ftype)

        return uri

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

        if vosclient is None:
            self.vosclient = vos.Client(cadc_short_cut=True)
        else:
            self.vosclient = vosclient

        self.cutout_calculator = CutoutCalculator(slice_rows, slice_cols)

    def _download_fits_file(self, uri, source_reading):
        # NOTE: ccd number is the extension, BUT Fits file extensions start at 1
        # Therefore ccd n = extension n + 1
        ccdnum = int(source_reading.obs.ccdnum)
        extension = str(ccdnum + 1)

        imgsize = source_reading.get_original_image_size()

        # TODO: clean this up.  Shouldn't have to handle this issue in
        # multiple places in the code.
        if source_reading.get_observation().is_fake():
            # We get the image from the CCD directory, it is not multi-extension,
            # and has already been corrected for inversion
            extension = 0
            inverted = False
        else:
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
          source_reading: ossos.astrom.SourceReading
            The reading to take a cutout around.
          in_memory: bool
            If True, the image is stored in memory without being written to
            disk.  If False, the image will be written to a temporary file.

        Returns:
          fitsimage: ossos.gui.image.DownloadedFitsImage
            The downloaded image, either in-memory or on disk as specified.
        """
        image_uri = self.resolver.resolve_image_uri(source_reading.obs)
        apcor_uri = self.resolver.resolve_apcor_uri(source_reading.obs)

        fits_str, converter = self._download_fits_file(image_uri, source_reading)
        apcor_str = self._download_apcor_file(apcor_uri)

        return DownloadedFitsImage(fits_str, apcor_str, converter,
                                   in_memory=in_memory)

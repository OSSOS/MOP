__author__ = "David Rusk <drusk@uvic.ca>"

import Queue
import threading

from ossos.download.downloads import DownloadRequest
from ossos.download.focus import SingletFocalPointCalculator
from ossos.gui import logger

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

        self._focal_point_calculator = SingletFocalPointCalculator()

    def start_downloading_workunit(self, workunit, image_loaded_callback=None):
        logger.debug("Starting to download workunit: %s" %
                     workunit.get_filename())

        self._maximize_workers()

        # Load up queue with downloadable items
        needs_apcor = workunit.is_apcor_needed()

        focal_points = []
        for source in workunit.get_unprocessed_sources():
            focal_points.extend(
                self._focal_point_calculator.calculate_focal_points(source))

        for focal_point in focal_points:
            self._work_queue.put(DownloadRequest(focal_point.reading,
                                                  focal_point.point,
                                                  needs_apcor,
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

            logger.debug("Created download worker thread (total=%d)" %
                         len(self._workers))

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

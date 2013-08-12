__author__ = "David Rusk <drusk@uvic.ca>"

import Queue
import threading

from ossos.downloads.focus import SingletFocalPointCalculator
from ossos.downloads.requests import DownloadRequest
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

    def submit_request(self, request):
        self._work_queue.put(request)
        self._maximize_workers()

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
            download_request = self.work_queue.get()
            self._idle = False

            try:
                self.do_download(download_request)
            except Exception as error:
                self.error_handler.handle_error(error, download_request)
            finally:
                # It is up to the error handler to requeue the downloadable
                # item if needed.
                self.work_queue.task_done()
                self._idle = True

    def do_download(self, download_request):
        download_request.execute(self.downloader)

    def stop(self):
        self._should_stop = True

    def is_stopped(self):
        return self._should_stop and self._idle

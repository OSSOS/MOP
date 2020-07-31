__author__ = "David Rusk <drusk@uvic.ca>"
import queue
import threading
from ..gui import logger
from ..gui import config

MAX_THREADS = config.read('APP.MAX_THREADS')


class AsynchronousDownloadManager(object):
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

        self._work_queue = queue.PriorityQueue()

        self._workers = []
        self._maximize_workers()

    def submit_request(self, request, priority=100):
        self._work_queue.put((priority, request))
        self._maximize_workers()

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
        self._workers = [thread for thread in self._workers if thread.is_alive()]

    def _all_workers_stopped(self):
        for worker in self._workers:
            if not worker.is_stopped():
                return False

        return True


class DownloadRequest(object):
    """
    Specifies an item (image and potentially related files) to be downloaded.
    """

    def __init__(self,
                 reading,
                 focus=None,
                 needs_apcor=False,
                 callback=None):
        """
        Constructor.

        Args:
          source_reading: ossos.astrom.SourceReading
            The reading which will be the focus of the downloaded image.
          focus: tuple(int, int)
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

        if focus is None:
            self.focus = reading.source_point
        else:
            self.focus = focus

    def __lt__(self, other):
        self.reading.get_exposure_number() < other.reading.get_exposure_number()

    def execute(self, downloader):
        cutout = downloader.download_cutout(self.reading,
                                            focus=self.focus,
                                            needs_apcor=self.needs_apcor)
        logger.debug("Got cutout: {}".format(cutout))
        if self.callback is not None:
            self.callback(cutout)


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
        download_request[1].execute(self.downloader)

    def stop(self):
        self._should_stop = True

    def is_stopped(self):
        return self._should_stop and self._idle

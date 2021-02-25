__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock

from ossos.astrom import SourceReading
from ossos.downloads.async_download import DownloadRequest
from ossos.downloads.async_download import DownloadThread
from ossos.downloads.cutouts import ImageCutoutDownloader
from ossos.downloads.cutouts.source import SourceCutout


class DownloadRequestTest(unittest.TestCase):
    def test_execute_downloads_cutout_and_calls_callback(self):
        reading = Mock(spec=SourceReading)
        callback = Mock()

        downloader = Mock(spec=ImageCutoutDownloader)
        cutout = Mock(spec=SourceCutout)
        downloader.download_cutout.return_value = cutout

        request = DownloadRequest(reading, focus=None, needs_apcor=False,
                                  callback=callback)

        request.execute(downloader)

        downloader.download_cutout.assert_called_once_with(reading,
                                                           focus=reading.source_point,
                                                           needs_apcor=False)

        callback.assert_called_once_with(cutout)


class DownloadThreadTest(unittest.TestCase):
    def test_do_download_executes_request(self):
        work_queue = Mock()
        downloader = Mock()
        errorhandler = Mock()

        thread = DownloadThread(work_queue, downloader, errorhandler)

        request = Mock(spec=DownloadRequest)
        thread.do_download(request)

        request.execute.assert_called_once_with(downloader)


if __name__ == '__main__':
    unittest.main()

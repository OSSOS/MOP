__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, patch

from pymop.io.imgaccess import (AsynchronousImageDownloadManager,
                                SerialImageDownloadThread)


class AsynchronousImageDownloadManagerTest(unittest.TestCase):
    def setUp(self):
        self.downloader = Mock()
        self.downloader.retrieve_image.return_value = (Mock(), Mock())

        self.undertest = AsynchronousImageDownloadManager(self.downloader)

    def mock_astrom_data(self, sources, observations):
        astrom_data = Mock()

        reading = Mock()
        reading.obs = Mock()
        source = [reading] * observations
        astrom_data.sources = [source] * sources

        return astrom_data

    @patch.object(SerialImageDownloadThread, "start")
    def test_do_load(self, mock_start_method):
        sources = 3
        observations = 2
        astrom_data = self.mock_astrom_data(sources, observations)

        self.undertest.start_download(astrom_data)

        mock_start_method.assert_called_once_with()

    @patch.object(SerialImageDownloadThread, "stop")
    @patch.object(SerialImageDownloadThread, "start")
    def test_stop_loading(self, mock_start_method, mock_stop_method):
        astrom_data = self.mock_astrom_data(3, 2)

        self.undertest.start_download(astrom_data)

        self.undertest.stop_download()
        mock_stop_method.assert_called_once_with()



if __name__ == '__main__':
    unittest.main()

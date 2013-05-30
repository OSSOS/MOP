__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock

from pymop.io.imgaccess import AsynchronousImageDownloadManager


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

    def test_do_load(self):
        sources = 3
        observations = 2
        astrom_data = self.mock_astrom_data(sources, observations)

        self.undertest.do_download = Mock()

        self.undertest.start_download(astrom_data)

        # XXX this has become a pretty weak test after refactoring...
        # need some way of testing the thread...
        self.undertest.do_download.assert_called_with(astrom_data)


if __name__ == '__main__':
    unittest.main()

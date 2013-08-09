__author__ = "David Rusk <drusk@uvic.ca>"

import os
import Queue
import tempfile
import unittest

from hamcrest import assert_that, equal_to, contains
from mock import Mock, call

import vos

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.download.async import DownloadThread
from ossos.download.cutouts import ImageCutoutDownloader
from ossos.download.downloads import DownloadableItem, DownloadedFitsImage
from ossos.gui.errorhandling import DownloadErrorHandler


class ImageSliceDownloaderTest(FileReadingTestCase):
    def setUp(self):
        self.image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"
        self.apcor_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/ccd15/1584431p15.apcor"

        self.downloadable_item = Mock(spec=DownloadableItem)
        self.downloadable_item.get_image_uri.return_value = self.image_uri
        self.downloadable_item.get_apcor_uri.return_value = self.apcor_uri
        self.downloadable_item.needs_apcor = True
        self.downloadable_item.get_focal_point.return_value = (75, 80)
        self.downloadable_item.get_extension.return_value = 19
        self.downloadable_item.get_full_image_size.return_value = (2000, 3000)
        self.downloadable_item.is_inverted.return_value = False

        self.vosclient = Mock(spec=vos.Client)
        self.undertest = ImageCutoutDownloader(slice_rows=100, slice_cols=50,
                                              vosclient=self.vosclient)

        # Mock vosclient to open a local file instead of one from vospace
        self.localfile = open(self.get_abs_path("data/testimg.fits"), "rb")
        self.apcorfile = tempfile.TemporaryFile("r+b")
        self.apcorfile.write("4 10 0.3 0.1")
        self.apcorfile.flush()
        self.apcorfile.seek(0)

        def choose_ret_val(*args, **kwargs):
            selected_file = None
            if self.image_uri in args:
                selected_file = self.localfile
            elif self.apcor_uri in args:
                selected_file = self.apcorfile
            else:
                self.fail("Unrecognized URI")

            selected_file.seek(0)

            return selected_file

        self.vosclient.open.side_effect = choose_ret_val

    def tearDown(self):
        self.localfile.close()
        self.apcorfile.close()

    def test_retrieve_sliced_image_in_memory(self):
        self.downloadable_item.in_memory = True

        fitsfile = self.undertest.download(self.downloadable_item)

        assert_that(self.vosclient.open.call_args_list, contains(
            call(self.image_uri, view="cutout", cutout="[19][50:100,30:130]"),
            call(self.apcor_uri, view="data")
        ))

        # This is just a test file, make sure we can read an expected value
        # it.  It won't have the right shape necessarily though.
        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_slice_in_file(self):
        self.downloadable_item.in_memory = False
        fitsfile = self.undertest.download(self.downloadable_item)

        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_in_file_removed_when_file_closed(self):
        self.downloadable_item.in_memory = False
        fitsfile = self.undertest.download(self.downloadable_item)

        assert_that(os.path.exists(fitsfile._tempfile.name))

        fitsfile.close()
        assert_that(not os.path.exists(fitsfile._tempfile.name))


class DownloadThreadTest(FileReadingTestCase):
    def setUp(self):
        self.downloader = Mock(spec=ImageCutoutDownloader)
        self.downloaded_image = Mock(spec=DownloadedFitsImage)
        self.downloader.download.return_value = self.downloaded_image
        self.error_handler = Mock(spec=DownloadErrorHandler)
        self.work_queue = Mock(spec=Queue.Queue)

        self.undertest = DownloadThread(self.work_queue, self.downloader,
                                        self.error_handler)

    def test_download_callback(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))

        source = astrom_data.get_sources()[0]
        reading = source.get_reading(0)

        callback = Mock()

        downloadable_item = DownloadableItem(reading, source, True, callback)

        self.undertest.do_download(downloadable_item)

        callback.assert_called_once_with(reading, self.downloaded_image)


if __name__ == '__main__':
    unittest.main()

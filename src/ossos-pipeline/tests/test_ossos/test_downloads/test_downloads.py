__author__ = "David Rusk <drusk@uvic.ca>"

import os
import tempfile
import unittest

from hamcrest import assert_that, equal_to, contains
from mock import Mock, call

import vos

from tests.base_tests import FileReadingTestCase
from ossos.astrom import SourceReading
from ossos.download.cutouts import ImageCutoutDownloader
from ossos.download.requests import DownloadRequest


class DownloadTest(FileReadingTestCase):
    def setUp(self):
        self.image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"
        self.apcor_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/ccd15/1584431p15.apcor"

        self.reading = Mock(spec=SourceReading)
        self.reading.get_image_uri.return_value = self.image_uri
        self.reading.get_apcor_uri.return_value = self.apcor_uri
        self.reading.get_extension.return_value = 19
        self.reading.get_original_image_size.return_value = (2000, 3000)
        self.reading.is_inverted.return_value = False

        self.needs_apcor = True
        self.focal_point = (75, 80)

        self.vosclient = Mock(spec=vos.Client)
        self.downloader = ImageCutoutDownloader(slice_rows=100, slice_cols=50,
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

    def make_request(self, callback=None, in_memory=True):
        return DownloadRequest(self.downloader, self.reading,
                               self.focal_point, self.needs_apcor, callback,
                               in_memory=in_memory)

    def test_retrieve_sliced_image_in_memory(self):
        request = self.make_request(in_memory=True)

        fitsfile = request.execute()

        assert_that(self.vosclient.open.call_args_list, contains(
            call(self.image_uri, view="cutout", cutout="[19][50:100,30:130]"),
            call(self.apcor_uri, view="data")
        ))

        # This is just a test file, make sure we can read an expected value
        # it.  It won't have the right shape necessarily though.
        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_slice_in_file(self):
        request = self.make_request(in_memory=False)
        fitsfile = request.execute()

        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_in_file_removed_when_file_closed(self):
        request = self.make_request(in_memory=False)
        fitsfile = request.execute()

        assert_that(os.path.exists(fitsfile._tempfile.name))

        fitsfile.close()
        assert_that(not os.path.exists(fitsfile._tempfile.name))

    def test_download_callback(self):
        callback = Mock()
        download = self.make_request(callback).execute()
        callback.assert_called_once_with(self.reading, download)


if __name__ == '__main__':
    unittest.main()

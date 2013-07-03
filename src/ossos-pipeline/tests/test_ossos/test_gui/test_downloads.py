__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os
import tempfile

from hamcrest import assert_that, equal_to, contains
from mock import Mock, call, patch

import vos

from tests.base_tests import FileReadingTestCase
from ossos.astrom import SourceReading, Observation
from ossos.gui.downloads import (ImageSliceDownloader, AsynchronousImageDownloadManager,
                                 SerialImageDownloadThread, VOSpaceResolver)


class ImageSliceDownloaderTest(FileReadingTestCase):
    def setUp(self):
        self.image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"
        self.apcor_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/ccd15/1584431p15.apcor"

        self.resolver = Mock()
        self.resolver.resolve_image_uri.return_value = self.image_uri
        self.resolver.resolve_apcor_uri.return_value = self.apcor_uri

        obs = Observation("1584431", "p", "18")
        obs.header = {"NAX1": "2000", "NAX2": "3000"}

        reading_x = 55
        reading_y = 60
        reading_x0 = 75
        reading_y0 = 80
        ref_x = 95
        ref_y = 100

        # Putting in 0's for don't cares
        self.source_reading = SourceReading(reading_x, reading_y, reading_x0,
                                            reading_y0, 0, 0, ref_x, ref_y, obs)

        self.vosclient = Mock(spec=vos.Client)
        self.undertest = ImageSliceDownloader(self.resolver,
                                              slice_rows=100, slice_cols=50,
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
        fitsfile = self.undertest.download(self.source_reading, in_memory=True)

        assert_that(self.vosclient.open.call_args_list, contains(
            call(self.image_uri, view="cutout", cutout="[19][50:100,30:130]"),
            call(self.apcor_uri, view="data")
        ))

        # This is just a test file, make sure we can read an expected value
        # it.  It won't have the right shape necessarily though.
        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_slice_in_file(self):
        fitsfile = self.undertest.download(self.source_reading, in_memory=False)

        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_in_file_removed_when_file_closed(self):
        fitsfile = self.undertest.download(self.source_reading, in_memory=False)

        assert_that(os.path.exists(fitsfile._tempfile.name))

        fitsfile.close()
        assert_that(not os.path.exists(fitsfile._tempfile.name))


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


class ResolverTest(unittest.TestCase):
    def setUp(self):
        self.resolver = VOSpaceResolver()

    def test_resolve_image_uri(self):
        observation = Observation("1584431", "p", "15")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p.fits"
        assert_that(self.resolver.resolve_image_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_apcor_uri(self):
        observation = Observation("1616681", "p", "22")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd22/1616681p22.apcor"
        assert_that(self.resolver.resolve_apcor_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_apcor_uri_single_digit_ccd(self):
        """Just double checking we don't run into trouble with leading zeros"""
        observation = Observation("1616681", "p", "05")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd05/1616681p05.apcor"
        assert_that(self.resolver.resolve_apcor_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_fake_image_uri(self):
        observation = Observation("1616682", "s", "24", fk="fk")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616682/ccd24/fk1616682s24.fits"
        assert_that(self.resolver.resolve_image_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_fake_apcor_uri(self):
        observation = Observation("1616682", "s", "24", fk="fk")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616682/ccd24/fk1616682s24.apcor"
        assert_that(self.resolver.resolve_apcor_uri(observation),
                    equal_to(expected_uri))


if __name__ == '__main__':
    unittest.main()

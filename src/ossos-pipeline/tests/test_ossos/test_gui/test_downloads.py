__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os
import tempfile

from hamcrest import assert_that, equal_to, contains, close_to
from mock import Mock, call

import vos

from tests.base_tests import FileReadingTestCase
from ossos.gui.image import DownloadedFitsImage
from ossos.astrom import SourceReading, Observation, AstromParser, Source
from ossos.gui.downloads import (ImageSliceDownloader, AsynchronousImageDownloadManager,
                                 VOSpaceResolver)


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

        self.source = Mock(spec=Source)

        self.vosclient = Mock(spec=vos.Client)
        self.undertest = ImageSliceDownloader(self.resolver,
                                              slice_rows=100, slice_cols=50,
                                              vosclient=self.vosclient)

        self.undertest.calculate_cutout_center = Mock(return_value=(75, 80))

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
        fitsfile = self.undertest.download(self.source_reading, self.source,
                                           in_memory=True)

        assert_that(self.vosclient.open.call_args_list, contains(
            call(self.image_uri, view="cutout", cutout="[19][50:100,30:130]"),
            call(self.apcor_uri, view="data")
        ))

        # This is just a test file, make sure we can read an expected value
        # it.  It won't have the right shape necessarily though.
        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_slice_in_file(self):
        fitsfile = self.undertest.download(self.source_reading, self.source,
                                           in_memory=False)

        assert_that(fitsfile.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_download_image_in_file_removed_when_file_closed(self):
        fitsfile = self.undertest.download(self.source_reading, self.source,
                                           in_memory=False)

        assert_that(os.path.exists(fitsfile._tempfile.name))

        fitsfile.close()
        assert_that(not os.path.exists(fitsfile._tempfile.name))


class ImageSliceDownloaderCalculationTest(FileReadingTestCase):
    def setUp(self):
        resolver = Mock(spec=VOSpaceResolver)
        vosclient = Mock(spec=vos.Client)

        self.undertest = ImageSliceDownloader(resolver, vosclient=vosclient)

    def test_calculate_center_point(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))

        source = astrom_data.get_sources()[0]
        delta = 0.0000001

        reading0 = source.get_reading(0)
        center0 = self.undertest.calculate_cutout_center(reading0, source)
        assert_that(center0[0], close_to(583.42, delta))
        assert_that(center0[1], close_to(408.46, delta))

        reading1 = source.get_reading(1)
        center1 = self.undertest.calculate_cutout_center(reading1, source)
        assert_that(center1[0], close_to(586.18, delta))
        assert_that(center1[1], close_to(408.63, delta))

        reading2 = source.get_reading(2)
        center2 = self.undertest.calculate_cutout_center(reading2, source)
        assert_that(center2[0], close_to(587.80, delta))
        assert_that(center2[1], close_to(407.98, delta))


class AsynchronousImageDownloadManagerTest(FileReadingTestCase):
    def setUp(self):
        self.downloader = Mock(spec=ImageSliceDownloader)
        self.downloaded_image = Mock(spec=DownloadedFitsImage)
        self.downloader.download.return_value = self.downloaded_image

        self.undertest = AsynchronousImageDownloadManager(self.downloader)

    def test_download_callback(self):
        astrom_data = AstromParser().parse(
            self.get_abs_path("data/1616681p22.measure3.cands.astrom"))

        source = astrom_data.get_sources()[0]
        reading = source.get_reading(0)

        callback = Mock()

        self.undertest.do_download((reading, source), False, callback)

        callback.assert_called_once_with(reading, self.downloaded_image)


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

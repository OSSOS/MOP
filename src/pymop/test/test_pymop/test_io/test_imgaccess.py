__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os
import tempfile

import vos
from hamcrest import assert_that, equal_to, contains
from mock import Mock, call

from test.base_tests import FileReadingTestCase
from pymop.io.parser import SourceReading, Observation
from pymop.io.imgaccess import (ImageSliceDownloader, CutoutCalculator)


class ImageSliceDownloaderTest(FileReadingTestCase):
    def setUp(self):
        self.image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"
        self.apcor_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/ccd15/1584431p15.apcor"

        self.resolver = Mock()
        self.resolver.resolve_image_uri.return_value = self.image_uri
        self.resolver.resolve_apcor_uri.return_value = self.apcor_uri

        obs = Observation("1584431", "p", "15")

        reading_x = 500
        reading_y = 600
        reading_x0 = 1000
        reading_y0 = 800

        # Putting in 0's for don't cares
        self.source_reading = SourceReading(reading_x, reading_y, reading_x0,
                                            reading_y0, 0, 0, obs)

        self.vosclient = Mock(spec=vos.Client)
        self.undertest = ImageSliceDownloader(self.resolver,
                                              slice_rows=100, slice_cols=200,
                                              vosclient=self.vosclient)

        # Mock vosclient to open a local file instead of one from vospace
        self.localfile = open(self.get_abs_path("data/testimg.fits"), "rb")
        self.apcorfile = tempfile.TemporaryFile("r+b")
        self.apcorfile.write("4 10 0.3 0.1")
        self.apcorfile.flush()
        self.apcorfile.seek(0)

        def choose_ret_val(*args, **kwargs):
            if self.image_uri in args:
                return self.localfile
            elif self.apcor_uri in args:
                return self.apcorfile
            else:
                self.fail("Unrecognized URI")

        self.vosclient.open.side_effect = choose_ret_val

    def tearDown(self):
        self.localfile.close()
        self.apcorfile.close()

    def test_retrieve_sliced_image_in_memory(self):
        fitsfile = self.undertest.download(self.source_reading, in_memory=True)

        # XXX is ccdnum actually the extension we want or is it something
        # standard like 2
        assert_that(self.vosclient.open.call_args_list, contains(
            call(self.image_uri, view="cutout", cutout="[16][900:1100,750:850]"),
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


class CutoutCalculatorTest(unittest.TestCase):
    def test_calc_cutout_internal(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500, 600))
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_calc_cutout_internal_str(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500, 600))
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_calc_cutout_internal_str_float(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500.00, 600.00))
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_build_cutout_str(self):
        self.calculator = CutoutCalculator(100, 200)

        cutout_str, _ = self.calculator.build_cutout_str(15, (500, 600))
        assert_that(cutout_str, equal_to("[15][400:600,550:650]"))

    def test_calc_cutout_internal_converter(self):
        self.calculator = CutoutCalculator(100, 200)

        _, converter = self.calculator.calc_cutout((500, 600))
        assert_that(converter.convert((400, 550)), equal_to((0, 0)))
        assert_that(converter.convert((600, 550)), equal_to((200, 0)))
        assert_that(converter.convert((400, 650)), equal_to((0, 100)))
        assert_that(converter.convert((600, 650)), equal_to((200, 100)))
        assert_that(converter.convert((500, 600)), equal_to((100, 50)))

    def test_build_cutout_str_converter(self):
        self.calculator = CutoutCalculator(100, 200)

        _, converter = self.calculator.build_cutout_str(15, (500, 600))
        assert_that(converter.convert((400, 550)), equal_to((0, 0)))
        assert_that(converter.convert((600, 550)), equal_to((200, 0)))
        assert_that(converter.convert((400, 650)), equal_to((0, 100)))
        assert_that(converter.convert((600, 650)), equal_to((200, 100)))
        assert_that(converter.convert((500, 600)), equal_to((100, 50)))

    def test_calc_cutout_boundary_x(self):
        self.calculator = CutoutCalculator(200, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((50, 400))
        assert_that(x0, equal_to(1))
        assert_that(x1, equal_to(201))
        assert_that(y0, equal_to(300))
        assert_that(y1, equal_to(500))

    def test_calc_cutout_boundary_y(self):
        self.calculator = CutoutCalculator(200, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((400, 50))
        assert_that(x0, equal_to(300))
        assert_that(x1, equal_to(500))
        assert_that(y0, equal_to(1))
        assert_that(y1, equal_to(201))

    def test_calc_cutout_boundary_x_converter(self):
        self.calculator = CutoutCalculator(200, 200)

        _, converter = self.calculator.build_cutout_str(15, (50, 400))
        assert_that(converter.convert((51, 400)), equal_to((50, 100)))
        assert_that(converter.convert((1, 300)), equal_to((0, 0)))

    def test_calc_cutout_boundary_x_converter(self):
        self.calculator = CutoutCalculator(200, 200)

        _, converter = self.calculator.build_cutout_str(15, (400, 50))
        assert_that(converter.convert((400, 51)), equal_to((100, 50)))
        assert_that(converter.convert((300, 1)), equal_to((0, 0)))


if __name__ == '__main__':
    unittest.main()

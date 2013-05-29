__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import vos
from hamcrest import assert_that, equal_to
from mock import Mock

from test.base_tests import FileReadingTestCase
from pymop.data_retrieval.image_retriever import ImageSliceRetriever
from pymop.data_retrieval.image_retriever import CutoutCalculator
from pymop.io.parser import SourceReading, Observation


class ImageRetrieverTest(FileReadingTestCase):
    def setUp(self):
        self.vosclient = Mock(spec=vos.Client)
        self.retriever = ImageSliceRetriever(slice_rows=100, slice_cols=200,
                                             vosclient=self.vosclient)

        # Mock vosclient to open a local file instead of one from vospace
        self.localfile = open(self.get_abs_path("data/testimg.fits"), "rb")
        self.vosclient.open.return_value = self.localfile

    def tearDown(self):
        self.localfile.close()

    def test_retrieve_sliced_image(self):
        image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"

        obs = Observation("1584431", "p", "15")
        reading_x = 500
        reading_y = 600
        # Putting in 0's for don't cares
        source_reading = SourceReading(reading_x, reading_y, 0, 0, 0, 0, obs)

        hdulist, _ = self.retriever.retrieve_image(image_uri, source_reading)

        # XXX is ccdnum actually the extension we want or is it something
        # standard like 2
        self.vosclient.open.assert_called_with(image_uri, view="cutout",
                                               cutout="[16][400:600,550:650]")

        # This is just a test file, make sure we can read an expected value
        # it.  It won't have the right shape necessarily though.
        assert_that(hdulist[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))


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


if __name__ == '__main__':
    unittest.main()

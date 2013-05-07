import unittest

import vos
from hamcrest import assert_that, equal_to
from mock import Mock

from mopgui.data_retrieval.image_retriever import ImageSliceRetriever
from mopgui.data_retrieval.image_retriever import CutoutCalculator


class ImageRetrieverTest(unittest.TestCase):
    def setUp(self):
        self.retriever = ImageSliceRetriever()

    @unittest.skip("TODO")
    def test_retrieve_sliced_image(self):
        image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p15.fits"
        target_source = "TODO"

        # Mock the vos Client
        self.retriever.vosclient = Mock(spec=vos.Client)

        self.retriever.vosclient.open.assert_called_with(image_uri, view="cutout",
                                                         cutout="TODO")

        image = self.retriever.retrieve_image(image_uri, target_source)
        assert_that(image.shape,
                    equal_to((self.retriever.slice_rows,
                              self.retriever.slice_cols)))


class CutoutCalculatorTest(unittest.TestCase):
    def test_calc_cutout_internal(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1) = self.calculator.calc_cutout((500, 600))
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_build_cutout_str(self):
        self.calculator = CutoutCalculator(100, 200)

        assert_that(self.calculator.build_cutout_str(15, (500, 600)),
                    equal_to("[15][400:600,550:650]"))


if __name__ == '__main__':
    unittest.main()

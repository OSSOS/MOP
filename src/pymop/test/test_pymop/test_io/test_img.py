__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import os

from mock import Mock
from hamcrest import assert_that, not_none, none, equal_to

from test.base_tests import FileReadingTestCase
from pymop.io.img import FitsImage


class FitsImageTest(FileReadingTestCase):
    def setUp(self):
        with open(self.get_abs_path("data/testimg.fits"), "rb") as fh:
            self.strdata = fh.read()

        self.coord_converter = Mock()

    def test_create_in_memory_image(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=True)

        # Breaking interface for testing purposes
        assert_that(fitsimage._hdulist, not_none())
        assert_that(fitsimage._tempfile, none())

    def test_create_on_disk_image(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=False)

        # Breaking interface for testing purposes
        assert_that(fitsimage._hdulist, none())
        assert_that(fitsimage._tempfile, not_none())

    def test_in_memory_as_file(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=True)
        assert_that(os.path.exists(fitsimage.as_file().name))

    def test_on_disk_as_hdulist(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=False)

        assert_that(fitsimage._hdulist, none())
        assert_that(fitsimage.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))
        assert_that(fitsimage._hdulist, not_none())

    def test_in_memory_as_hdulist(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=True)
        assert_that(fitsimage.as_hdulist()[0].header["FILENAME"],
                    equal_to("u5780205r_cvt.c0h"))

    def test_on_disk_as_file(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=False)
        assert_that(os.path.exists(fitsimage.as_file().name))

    def test_close(self):
        fitsimage = FitsImage(self.strdata, self.coord_converter, in_memory=True)
        as_file = fitsimage.as_file()

        fitsimage.close()

        assert_that(not os.path.exists(as_file.name))


if __name__ == '__main__':
    unittest.main()

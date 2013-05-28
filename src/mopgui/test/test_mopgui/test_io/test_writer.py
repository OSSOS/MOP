__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length

from mopgui.io import writer


class MPCWriterTest(unittest.TestCase):
    def setUp(self):
        self.outputfile = tempfile.TemporaryFile()
        self.undertest = writer.MPCWriter(self.outputfile)

    def tearDown(self):
        self.outputfile.close()

    def read_outputfile(self):
        self.outputfile.seek(0)
        return self.outputfile.read()

    def test_write_line_valid_inputs(self):
        self.undertest.write_line("12345",
                                  "1234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  "26.683336700", # 01 46 44.001
                                  "29.220353200", # +29 13 13.27
                                  "123.5",
                                  "A",
                                  "523")

        expected = "123451234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    @unittest.skip("TODO: have test methods checking each input")
    def test_write_line_invalid_inputs(self):
        pass

    def test_format_ra(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        formatted_ra, _ = writer.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_ra, equal_to("00 42 44.299"))

    def test_format_dec(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        _, formatted_dec = writer.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_dec, equal_to("+41 16 09.01"))

    def test_format_ra_dec_strings(self):
        formatted_ra, formatted_dec = writer.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_ra, equal_to("00 42 44.299"))
        assert_that(formatted_dec, equal_to("+41 16 09.01"))


if __name__ == '__main__':
    unittest.main()

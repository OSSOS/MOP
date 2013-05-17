__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length

from mopgui.io.writer import MPCWriter


class MPCWriterTest(unittest.TestCase):
    def setUp(self):
        self.outputfile = tempfile.TemporaryFile()
        self.undertest = MPCWriter(self.outputfile)

    def tearDown(self):
        self.outputfile.close()

    @unittest.skip("TODO: in progress")
    def test_write_line_valid_inputs(self):
        self.undertest.write_line("12345",
                                  "1234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  "26.683336700",
                                  "29.220353200",
                                  "123.5",
                                  "A",
                                  "523")

        expected = "123451234567*MN2012 10 21.40516026.68333670029.220353200         123.5A      523\n"

        actual = self.outputfile.read()
        print "Actual: ", actual
        assert_that(actual.endswith("\n"))
        actual = actual[:-1]

        assert_that(actual, has_length(80))
        assert_that(actual, equal_to(expected))

    @unittest.skip("TODO: have test methods checking each input")
    def test_write_line_invalid_inputs(self):
        pass


if __name__ == '__main__':
    unittest.main()

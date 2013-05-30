__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length

from pymop.io import writer


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
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  "26.683336700", # 01 46 44.001
                                  "29.220353200", # +29 13 13.27
                                  "123.5",
                                  "A",
                                  "523")

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_inputs_numeric(self):
        self.undertest.write_line("12345",
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  26.683336700, # 01 46 44.001
                                  29.220353200, # +29 13 13.27
                                  123.5,
                                  "A",
                                  523)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag(self):
        self.undertest.write_line("12345",
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  26.683336700, # 01 46 44.001
                                  29.220353200, # +29 13 13.27
                                  22.5211,
                                  "A",
                                  523)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag_str(self):
        self.undertest.write_line("12345",
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  26.683336700,
                                  29.220353200,
                                  "22.5211", # In string form
                                  "A",
                                  523)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_date_short(self):
        self.undertest.write_line("12345",
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2013 04 09.36658",
                                  "26.683336700", # 01 46 44.001
                                  "29.220353200", # +29 13 13.27
                                  "123.5",
                                  "A",
                                  "523")

        expected = "12345A234567*MN2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_pad_empties(self):
        self.undertest.write_line("12345",
                                  "A234567",
                                  "",
                                  "",
                                  "",
                                  "2013 04 09.36658",
                                  "26.683336700", # 01 46 44.001
                                  "29.220353200", # +29 13 13.27
                                  "123.5",
                                  "A",
                                  "523")

        expected = "12345A234567   2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_empty_minor_planet_number(self):
        self.undertest.write_line("",
                                  "A234567",
                                  "*",
                                  "M",
                                  "N",
                                  "2012 10 21.405160",
                                  "26.683336700", # 01 46 44.001
                                  "29.220353200", # +29 13 13.27
                                  "123.5",
                                  "A",
                                  "523")

        expected = "     A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_MPCFormatException_message(self):
        ex = writer.MPCFieldFormatException("Note1", "must be 1 character", "AB")
        assert_that(ex.message,
                    equal_to("Field Note1: must be 1 character; but was AB"))

    def test_write_line_minor_planet_number_invalid(self):
        args = ["123456", # Too long
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_provisional_name_too_long(self):
        args = ["12345",
                "A2345678", # Too long
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_provisional_name_too_short(self):
        args = ["12345",
                "", # Too short
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_provisional_name_doesnt_start_with_letter(self):
        args = ["12345",
                "9abc", # Doesn't start with letter
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_discovery_asterisk_invalid(self):
        args = ["12345",
                "A234567",
                "**",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_note1_too_long(self):
        args = ["12345",
                "A234567",
                "*",
                "MM",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_note2_too_long(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "NN",
                "2012 10 21.405160",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_date_wrong_format(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "10 21.405160 2012",
                "26.683336700",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_ra_not_numeric(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.F",
                "29.220353200",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_dec_not_numeric(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.F",
                "123.5",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_obs_mag_too_long(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.683336700",
                "123.54",
                "A",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_obs_mag_not_numeric(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.683336700",
                "12.5B",
                "A",
                "523"]

        self.assertRaises(ValueError,
                          self.undertest.write_line,
                          *args)

    def test_write_line_no_band(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.683336700",
                "123.5",
                "",
                "523"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

    def test_write_line_observatory_code_too_long(self):
        args = ["12345",
                "A234567",
                "*",
                "M",
                "N",
                "2012 10 21.405160",
                "26.683336700",
                "29.683336700",
                "123.5",
                "A",
                "5234"]

        self.assertRaises(writer.MPCFieldFormatException,
                          self.undertest.write_line,
                          *args)

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

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length
from astropy.time.core import Time


from ossos.astrom import SourceReading, Observation
from ossos import mpc

Time.FORMATS['mpc'] = mpc.TimeMPC

class ObservationTest(unittest.TestCase):
    def test_default_line(self):
        expected = "     K04K18V**C2004 05 24.36017 15 06 36.12 -18 56 49.5          23.5 g      568"
        actual = str(mpc.Observation(provisional_name="K04K18V",
                                          discovery=True,
                                          note1='*',
                                          note2='C',
                                          date="2004 05 24.36017",
                                          ra="15 06 36.12",
                                          dec="-18 56 49.5",
                                          mag=23.5,
                                          band='g',
                                          observatory_code=568))
        self.assertEquals(expected, actual)

    def test_format_line(self):
        actual = str(mpc.Observation("12345","A234567","*","M","N","2012 10 21.405160",
                                      "26.683336700", # 01 46 44.001
                                      "29.220353200", # +29 13 13.27
                                      "12.4",
                                      "A",
                                      "523"))

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         12.4 A      523"

        assert_that(actual, has_length(80))
        self.assertEquals(actual, expected)

class TimeMPCTest(unittest.TestCase):

    def test_time_formatting(self):
        mpc_time = "2000 01 01.000001"
        iso_time = "2000-01-01 00:00:00.0864"
        t1 = Time(mpc_time, format='mpc', in_subfmt='mpc', scale='utc', precision=6)
        t2 = Time(iso_time, format='iso', scale='utc', precision=4)
        t3 = t2.replicate(format='mpc')
        t3.precision=6
        self.assertEquals(str(t1),mpc_time)
        self.assertEquals(t2.jd, t1.jd)
        self.assertEquals(str(t3), mpc_time)


class MPCNoteTest(unittest.TestCase):

    def test_note1_code_large_numeric(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='10', note_type="Note1")

    def test_note2_code_invalid(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='1', note_type="Note2")

    def test_note1_get_long_description(self):
        self.assertEquals(mpc.MPCNote(code="H",note_type="Note1").long,mpc.MPCNOTES["Note1"]["H"])

    def test_note2_set_correctly(self):
        self.assertEquals(str(mpc.MPCNote(code="C",note_type="Note2")),"C")



class MPCWriterTest(unittest.TestCase):
    def setUp(self):
        self.outputfile = tempfile.TemporaryFile()
        self.undertest = mpc.MPCWriter(self.outputfile)

    def tearDown(self):
        self.outputfile.close()

    def read_outputfile(self):
        self.outputfile.seek(0)
        return self.outputfile.read()

    def test_write_line_valid_inputs(self):
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("12345",
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
        self.undertest.write_mpc_line("",
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

    def test_write_phot_failure(self):
        self.undertest.write_mpc_line("12345",
                                      "A234567",
                                      "*",
                                      "M",
                                      "N",
                                      "2012 10 21.405160",
                                      "26.683336700", # 01 46 44.001
                                      "29.220353200", # +29 13 13.27
                                      "",
                                      "",
                                      "523",
                                      phot_failure=True)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27                     523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))


    def test_MPCFormatException_message(self):
        ex = mpc.MPCFieldFormatError("Note1", "must be 1 character", "AB")
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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
                "123456",
                "A",
                "523"]

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
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

        self.assertRaises(mpc.MPCFieldFormatError,
                          self.undertest.write_mpc_line,
                          *args)

    def test_write_comment(self):
        obs = Observation("1234567", "p", "00")
        reading = SourceReading(334.56, 884.22, 335.56, 885.22, 0, 0,
                                335.56, 885.22, obs)

        self.undertest.write_comment(reading, "Something fishy.")

        assert_that(self.read_outputfile(),
                    equal_to("# 1234567p00 334.56 884.22 Something fishy.\n"))

    def test_write_rejection_line(self):
        self.undertest.write_rejection_line("2012 10 21.405160",
                                            "26.683336700", # 01 46 44.001
                                            "29.220353200", # +29 13 13.27
        )

        expected = "!              2012 10 21.40516001 46 44.001+29 13 13.27                        \n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False)
        obs = Observation("1234567", "p", "00")
        reading = SourceReading(334.56, 884.22, 335.56, 885.22, 0, 0,
                                335.56, 885.22, obs)

        self.undertest.write_comment(reading, "Something fishy.")

        assert_that(self.read_outputfile(), equal_to(""))

        self.undertest.write_mpc_line("12345",
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

        assert_that(self.read_outputfile(), equal_to(""))

        self.undertest.write_rejection_line("2012 10 21.405160",
                                            "26.683336700", # 01 46 44.001
                                            "29.220353200", # +29 13 13.27
        )

        assert_that(self.read_outputfile(), equal_to(""))

        expected_comment = "# 1234567p00 334.56 884.22 Something fishy.\n"
        expected_mpcline = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"
        expected_reject_line = "!              2012 10 21.40516001 46 44.001+29 13 13.27                        \n"

        self.undertest.flush()
        assert_that(self.read_outputfile(),
                    equal_to(expected_comment + expected_mpcline + expected_reject_line))

    def test_format_ra(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        formatted_ra, _ = mpc.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_ra, equal_to("00 42 44.299"))

    def test_format_dec(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        _, formatted_dec = mpc.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_dec, equal_to("+41 16 09.01"))

    def test_format_ra_dec_strings(self):
        formatted_ra, formatted_dec = mpc.format_ra_dec(10.68458, 41.26917)
        assert_that(formatted_ra, equal_to("00 42 44.299"))
        assert_that(formatted_dec, equal_to("+41 16 09.01"))


if __name__ == '__main__':
    unittest.main()

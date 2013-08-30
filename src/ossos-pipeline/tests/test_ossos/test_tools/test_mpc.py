__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length, contains
from astropy.time.core import Time

from ossos import mpc

Time.FORMATS['mpc'] = mpc.TimeMPC


class ObservationTest(unittest.TestCase):
    def test_create_from_line(self):
        expected = '     NONE     C2009 02 23.62578512 32 05.249+07 08 55.70         30.00g      568'
        self.assertEquals(expected, str(mpc.Observation.from_string(expected)))

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
        actual = str(mpc.Observation("12345", "A234567", "*", "H", "N", "2012 10 21.405160",
                                     "26.683336700", # 01 46 44.001
                                     "29.220353200", # +29 13 13.27
                                     "12.4",
                                     "A",
                                     "523"))

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         12.4 A      523"

        assert_that(actual, has_length(80))
        self.assertEquals(actual, expected)

    def test_MPCFormatException_message(self):
        ex = mpc.MPCFieldFormatError("Note1", "must be 1 character", "AB")
        assert_that(ex.message,
                    equal_to("Field Note1: must be 1 character; but was AB"))

    def test_minor_planet_number_invalid(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="123456", # Too long
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_provisional_name_too_long(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A2345678", # Too long
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_provisional_name_too_short(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="", # Too short
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_provisional_name_doesnt_start_with_letter(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="9abc", # Doesn't start with letter
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_discovery_asterisk_invalid(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="**",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_note1_too_long(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="HH",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_note2_too_long(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="NN",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_date_wrong_format(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="10 21.405160 2012",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_ra_not_numeric(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.F",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_dec_not_numeric(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.F",
                          mag="123.5",
                          band="A",
                          observatory_code="523")

    def test_observatory_code_too_long(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
                          minor_planet_number="12345",
                          provisional_name="A234567",
                          discovery="*",
                          note1="H",
                          note2="N",
                          date="2012 10 21.405160",
                          ra="26.683336700",
                          dec="29.220353200",
                          mag="123.5",
                          band="A",
                          observatory_code="5234")


class TimeMPCTest(unittest.TestCase):
    def test_time_formatting(self):
        mpc_time = "2000 01 01.000001"
        iso_time = "2000-01-01 00:00:00.0864"
        t1 = Time(mpc_time, format='mpc', scale='utc', precision=6)
        t2 = Time(iso_time, format='iso', scale='utc', precision=6)
        t3 = t2.replicate(format='mpc')
        t3.precision = 6
        self.assertEquals(mpc_time, str(t1))
        self.assertEquals(t2.jd, t1.jd)
        self.assertEquals(mpc_time, str(t3))


class MPCNoteTest(unittest.TestCase):
    def test_note1_code_large_numeric(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='10', note_type="Note1")

    def test_note2_code_invalid(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='1', note_type="Note2")

    def test_note1_get_long_description(self):
        self.assertEquals(mpc.MPCNote(code="H", note_type="Note1").long, mpc.MPCNOTES["Note1"]["H"])

    def test_note2_set_correctly(self):
        self.assertEquals(str(mpc.MPCNote(code="C", note_type="Note2")), "C")


class MPCWriterTest(unittest.TestCase):
    def setUp(self):
        self.outputfile = tempfile.TemporaryFile()
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=True,
                                       include_comments=False)

    def tearDown(self):
        self.outputfile.close()

    def read_outputfile(self):
        self.outputfile.seek(0)
        return self.outputfile.read()

    def test_write_line_valid_inputs(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_inputs_numeric(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700, # 01 46 44.001
                              dec=29.220353200, # +29 13 13.27
                              mag=123.5,
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700, # 01 46 44.001
                              dec=29.220353200, # +29 13 13.27
                              mag=22.5211,
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag_str(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700,
                              dec=29.220353200,
                              mag="22.5211", # In string form
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_date_short(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2013 04 09.36658",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "12345A234567*HN2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_pad_empties(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              note1="",
                              note2="",
                              date="2013 04 09.36658",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "12345A234567*  2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_empty_minor_planet_number(self):
        obs = mpc.Observation(minor_planet_number="",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_obs_mag_too_long(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",
                              dec="29.220353200",
                              mag="12.3456",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         12.35A      523\n"
        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_phot_failure(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="",
                              band="",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27                     523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_comment(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=True,
                                       include_comments=True)
        frame = "1234567p00"
        xpos = 334.56
        ypos = 884.22
        comment = "Something fishy."
        comment = "Something fishy."
        obs = mpc.Observation(minor_planet_number="",
                              provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523",
                              comment=comment,
                              frame=frame,
                              xpos=xpos,
                              ypos=ypos)

        self.undertest.write(obs)

        expected = ("     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27"
                    "         123.5A      523 1234567p00 A234567 LH"
                    "  334.6  884.2 123.50  UUUU % Something fishy.\n")
        assert_that(self.read_outputfile(), equal_to(expected))

    def test_write_rejection_line(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=True,
                                       include_comments=True)
        frame = "1234567p00"
        xpos = 334.56
        ypos = 884.22
        comment = "Something fishy."
        obs = mpc.Observation(provisional_name="A234567",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              comment=comment,
                              frame=frame,
                              xpos=xpos,
                              ypos=ypos)

        obs.null_observation = True

        self.undertest.write(obs)

        expected = ("!    A234567   2012 10 21.40516001 46 44.001+29 13 13.27"
                    "         -1   r      568 1234567p00 A234567 Z"
                    "   334.6  884.2   UUUU % Something fishy.\n")

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, equal_to(expected))

    def test_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs1)

        assert_that(self.read_outputfile(), equal_to(""))

        obs2 = mpc.Observation(date="2012 10 22.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
        )

        obs2.null_observation = True

        self.undertest.write(obs2)

        assert_that(self.read_outputfile(), equal_to(""))

        expected_mpcline = "12345A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"
        expected_reject_line = "!              2012 10 22.40516001 46 44.001+29 13 13.27         -1   r      568\n"

        self.undertest.flush()
        assert_that(self.read_outputfile(),
                    equal_to(expected_mpcline + expected_reject_line))

    def test_written_obs_sorted_chronologically_on_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs2 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs3 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 12 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs3)
        self.undertest.write(obs1)
        self.undertest.write(obs2)

        assert_that(self.undertest.get_chronological_buffered_observations(),
                    contains(obs1, obs2, obs3))

    def test_discovery_asterisk_for_first_flushed(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs2 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs2)
        self.undertest.write(obs1)

        self.undertest.flush()

        expected_first_line = ("12345A234567*HN2012 10 21.40516001 46 44.001"
                               "+29 13 13.27         123.5A      523\n")
        expected_second_line = ("12345A234567 HN2012 11 21.40516001 46 44.001"
                               "+29 13 13.27         123.5A      523\n")

        assert_that(self.read_outputfile(),
                    equal_to(expected_first_line + expected_second_line))

    def test_rejected_line_not_discovery_asterisk(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(provisional_name="A234567",
                               date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               observatory_code="523")
        obs1.null_observation = True

        obs2 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs2)
        self.undertest.write(obs1)

        self.undertest.flush()

        expected_first_line = ("!    A234567   2012 10 21.40516001 46 44.001"
                               "+29 13 13.27         -1   r      523\n")
        expected_second_line = ("12345A234567*HN2012 11 21.40516001 46 44.001"
                                "+29 13 13.27         123.5A      523\n")

        assert_that(self.read_outputfile(),
                    equal_to(expected_first_line + expected_second_line))

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

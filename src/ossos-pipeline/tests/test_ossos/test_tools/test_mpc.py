__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length
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
        actual = str(mpc.Observation("12345", "A234567", "*", "M", "N", "2012 10 21.405160",
                                     "26.683336700", # 01 46 44.001
                                     "29.220353200", # +29 13 13.27
                                     "12.4",
                                     "A",
                                     "523"))

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         12.4 A      523"

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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
                          note1="MM",
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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
                          note1="M",
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
        self.undertest = mpc.MPCWriter(self.outputfile)

    def tearDown(self):
        self.outputfile.close()

    def read_outputfile(self):
        self.outputfile.seek(0)
        return self.outputfile.read()

    def test_write_line_valid_inputs(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_inputs_numeric(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700, # 01 46 44.001
                              dec=29.220353200, # +29 13 13.27
                              mag=123.5,
                              band="A",
                              observatory_code=523)

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700, # 01 46 44.001
                              dec=29.220353200, # +29 13 13.27
                              mag=22.5211,
                              band="A",
                              observatory_code=523)

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_round_obs_mag_str(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700,
                              dec=29.220353200,
                              mag="22.5211", # In string form
                              band="A",
                              observatory_code=523)

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         22.52A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_date_short(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2013 04 09.36658",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_valid_pad_empties(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="",
                              note1="",
                              note2="",
                              date="2013 04 09.36658",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567   2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_line_empty_minor_planet_number(self):
        obs = mpc.Observation(minor_planet_number="",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "     A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_obs_mag_too_long(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",
                              dec="29.220353200",
                              mag="12.3456",
                              band="A",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         12.35A      523\n"
        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_phot_failure(self):
        obs = mpc.Observation(minor_planet_number="12345",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="",
                              band="",
                              observatory_code="523")

        self.undertest.write_mpc_line(obs)

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27                     523\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, has_length(81))
        assert_that(actual, equal_to(expected))

    def test_write_comment(self):
        comment = "1234567p00 334.56 884.22 Something fishy."
        obs = mpc.Observation(minor_planet_number="",
                              provisional_name="A234567",
                              discovery="*",
                              note1="M",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523",
                              comment=comment)

        self.undertest.write_mpc_line(obs)

        expected = ("     A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27"
                    "         123.5A      523 1234567p00 334.56 884.22 Something fishy.\n")
        assert_that(self.read_outputfile(), equal_to(expected))

    def test_write_rejection_line(self):
        comment = "1234567p00 334.56 884.22 Something fishy."
        obs = mpc.Observation(date="2012 10 21.405160",
                              ra="26.683336700", # 01 46 44.001
                              dec="29.220353200", # +29 13 13.27
                              comment=comment)

        obs.null_observation = True

        self.undertest.write_mpc_line(obs)

        expected = "!              2012 10 21.40516001 46 44.001+29 13 13.27         0.0  r      568 1234567p00 334.56 884.22 Something fishy.\n"

        actual = self.read_outputfile()

        assert_that(actual.endswith("\n"))
        assert_that(actual, equal_to(expected))

    def test_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False)

        obs1 = mpc.Observation(minor_planet_number="12345",
                               provisional_name="A234567",
                               discovery="*",
                               note1="M",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write_mpc_line(obs1)

        assert_that(self.read_outputfile(), equal_to(""))

        obs2 = mpc.Observation(date="2012 10 21.405160",
                               ra="26.683336700", # 01 46 44.001
                               dec="29.220353200", # +29 13 13.27
        )

        obs2.null_observation = True

        self.undertest.write_mpc_line(obs2)

        assert_that(self.read_outputfile(), equal_to(""))

        expected_mpcline = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"
        expected_reject_line = "!              2012 10 21.40516001 46 44.001+29 13 13.27         0.0  r      568\n"

        self.undertest.flush()
        assert_that(self.read_outputfile(),
                    equal_to(expected_mpcline + expected_reject_line))

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

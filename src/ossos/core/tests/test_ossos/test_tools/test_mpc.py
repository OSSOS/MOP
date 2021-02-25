import os
import numpy
import unittest
import tempfile
from ossos import mpc
from astropy.time import Time
from ossos import util
__author__ = "David Rusk <drusk@uvic.ca>"


class ObservationTest(unittest.TestCase):

    def test_ast_line(self):
        """Test that a dbase .ast file line is correctly parsed"""

        ast_line = '     O14BH54  C2013 09 01.54512 01 33 24.655+11 47 59.22         23.3 r' \
                   '      568 20130801_568_1 20151001 0010400000                      ' \
                   'O   1651194p29 H54         Y  1939.58  482.54 0.20 0 23.29 0.07 % A Comment '
        frame = "1651194p29"

        parsed = mpc.Observation.from_string(ast_line)
        self.assertEqual(frame, parsed.comment.frame)

    def test_create_from_line(self):
        expected = '     NONE     C2009 02 23.62578512 32 05.249+07 08 55.70         30.0 g      568'
        self.assertEqual(expected, str(mpc.Observation.from_string(expected)))

    def test_default_line(self):
        expected = "     K04K18V**C2004 05 24.36017 15 06 36.120-18 56 49.50         23.5 g      568"
        actual = str(mpc.Observation(provisional_name="K04K18V",
                                     discovery="*",
                                     note1='*',
                                     note2='C',
                                     date="2004 05 24.36017",
                                     ra="15 06 36.12",
                                     dec="-18 56 49.5",
                                     mag=23.5,
                                     band='g',
                                     observatory_code=568))
        self.assertEqual(expected, actual)

    def test_mpcdatabase_line(self):
        mpc_line = "08611K01QT7T  C2004 05 29.32355 22 01 19.57 -11 03 12.4                ok8659304"
        obs = mpc.Observation.from_string(mpc_line)
        self.assertEqual(str(obs.ra), "22 01 19.570")
        self.assertEqual(obs.provisional_name, "K01QT7T")
        self.assertEqual(obs.minor_planet_number, 8611)
        self.assertEqual(obs.minor_planet_number, "08611")

    def test_format_line(self):
        actual = str(mpc.Observation(None, None, "A234567", "*", "H", "N", "2012 10 21.405160",
                                     "26.683336700",  # 01 46 44.001
                                     "29.220353200",  # +29 13 13.27
                                     "12.4",
                                     "A",
                                     "523"))

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         12.4 A      523"

        self.assertEqual(len(actual), 80)
        self.assertEqual(actual, expected)

    def test_MPCFormatException_message(self):
        ex = mpc.MPCFieldFormatError("Note1", "must be 1 character", "AB")
        self.assertEqual(ex.message, "Field Note1: must be 1 character; but was AB")

    def test_note2_too_long(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.Observation,
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
        self.assertEqual(mpc_time, str(t1))
        self.assertEqual(t2.jd, t1.jd)
        self.assertEqual(mpc_time, str(t3))


class MPCNoteTest(unittest.TestCase):
    def test_note1_code_large_numeric(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='10', note_type="Note1")

    def test_note2_code_invalid(self):
        self.assertRaises(mpc.MPCFieldFormatError,
                          mpc.MPCNote, code='1', note_type="Note2")

    def test_note1_get_long_description(self):
        self.assertEqual(mpc.MPCNote(code="H", note_type="Note1").long, mpc.MPCNOTES["Note1"]["H"])

    def test_note2_set_correctly(self):
        self.assertEqual(str(mpc.MPCNote(code="C", note_type="Note2")), "C")


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
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        self.assertTrue(actual.endswith("\n"))
        self.assertEqual(len(actual), 81)
        self.assertEqual(actual, expected)

    def test_write_line_valid_inputs_numeric(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700,  # 01 46 44.001
                              dec=29.220353200,  # +29 13 13.27
                              mag=123.5,
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        self.assertTrue(actual.endswith("\n"))
        self.assertEqual(len(actual), 81)
        self.assertEqual(actual, expected)

    def test_write_line_round_obs_mag(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700,  # 01 46 44.001
                              dec=29.220353200,  # +29 13 13.27
                              mag=22.5211,
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         22.5 A      523\n"

        actual = self.read_outputfile()

        self.assertTrue(actual.endswith("\n"))
        self.assertEqual(len(actual), 81)
        self.assertEqual(actual, expected)

    def test_write_line_round_obs_mag_str(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra=26.683336700,
                              dec=29.220353200,
                              mag="22.5211",  # In string form
                              band="A",
                              observatory_code=523)

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         22.5 A      523\n"

        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_write_line_valid_date_short(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2013 04 09.36658",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*HN2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_write_line_valid_pad_empties(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery=True,
                              note1="",
                              note2="",
                              date="2013 04 09.36658",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*  2013 04 09.36658 01 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()
        self.assertEqual(actual, expected)

    def test_write_line_empty_minor_planet_number(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              mag="123.5",
                              band="A",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_obs_mag_too_long(self):
        obs = mpc.Observation(provisional_name="A234567",
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

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         12.3 A      523\n"
        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_write_phot_failure(self):
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              mag="",
                              band="",
                              observatory_code="523")

        self.undertest.write(obs)

        expected = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27                     523\n"

        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_write_comment(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=True,
                                       include_comments=True)
        frame = "1234567p00"
        xpos = 334.56
        ypos = 884.22
        comment = "Something fishy."
        obs = mpc.Observation(provisional_name="A234567",
                              discovery="*",
                              note1="H",
                              note2="N",
                              date="2012 10 21.405160",
                              ra="26.683336700",
                              dec="29.220353200",
                              mag="23.50",
                              band="A",
                              observatory_code="523",
                              comment=comment,
                              frame=frame,
                              xpos=xpos,
                              ypos=ypos)

        self.undertest.write(obs)

        expected = ("     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27"
                    "         23.5 A      523 O   1234567p00 A234567     ZH"
                    "  334.56  884.22 0.20 0 23.50 ---- % Something fishy.\n")
        self.assertEqual(self.read_outputfile(), expected)

    def test_write_rejection_line(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=True,
                                       include_comments=True)
        frame = "1234567p00"
        xpos = 334.56
        ypos = 884.22
        comment = "Something fishy."
        obs = mpc.Observation(provisional_name="A234567",
                              date="2012 10 21.405160",
                              ra="26.683336700",  # 01 46 44.001
                              dec="29.220353200",  # +29 13 13.27
                              comment=comment,
                              frame=frame,
                              xpos=xpos,
                              ypos=ypos)

        obs.null_observation = True

        self.undertest.write(obs)

        expected = ("!    A234567   2012 10 21.40516001 46 44.001+29 13 13.27"
                    "                     568 O   1234567p00 A234567     Z"
                    "   334.56  884.22 0.20 0 ----- ---- % Something fishy.\n")

        actual = self.read_outputfile()

        self.assertEqual(actual, expected)

    def test_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs1)

        self.assertEqual(self.read_outputfile(), "")

        obs2 = mpc.Observation(date="2012 10 22.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               )

        obs2.null_observation = True

        self.undertest.write(obs2)

        self.assertEqual(self.read_outputfile(), "")

        expected_mpcline = "     A234567*HN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"
        expected_reject_line = "!              2012 10 22.40516001 46 44.001+29 13 13.27                     568\n"

        self.undertest.flush()
        self.assertEqual(self.read_outputfile(), expected_mpcline + expected_reject_line)

    def test_written_obs_sorted_chronologically_on_flush(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs2 = mpc.Observation(provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs3 = mpc.Observation(provisional_name="A234567",
                               discovery="*",
                               note1="H",
                               note2="N",
                               date="2012 12 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs3)
        self.undertest.write(obs1)
        self.undertest.write(obs2)

        self.assertTrue(obs1 in self.undertest.get_chronological_buffered_observations())
        self.assertTrue(obs2 in self.undertest.get_chronological_buffered_observations())
        self.assertTrue(obs3 in self.undertest.get_chronological_buffered_observations())

    def test_discovery_asterisk_for_first_flushed(self):
        self.undertest = mpc.MPCWriter(self.outputfile, auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(provisional_name="A234567",
                               note1="H",
                               note2="N",
                               date="2012 10 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")
        obs2 = mpc.Observation(provisional_name="A234567",
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="123.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs2)
        self.undertest.write(obs1)

        self.undertest.flush()

        expected_first_line = ("     A234567*HN2012 10 21.40516001 46 44.001"
                               "+29 13 13.27         123.5A      523\n")
        expected_second_line = ("     A234567 HN2012 11 21.40516001 46 44.001"
                                "+29 13 13.27         123.5A      523\n")

        self.assertEqual(self.read_outputfile(), expected_first_line + expected_second_line)

    def test_rejected_line_not_discovery_asterisk(self):
        self.undertest = mpc.MPCWriter(self.outputfile,
                                       auto_flush=False,
                                       include_comments=False)

        obs1 = mpc.Observation(provisional_name="A234567",
                               date="2012 10 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               observatory_code="523")
        obs1.null_observation = True

        obs2 = mpc.Observation(provisional_name="A234567",
                               discovery=True,
                               note1="H",
                               note2="N",
                               date="2012 11 21.405160",
                               ra="26.683336700",  # 01 46 44.001
                               dec="29.220353200",  # +29 13 13.27
                               mag="23.5",
                               band="A",
                               observatory_code="523")

        self.undertest.write(obs2)

        self.undertest.write(obs1)

        self.undertest.flush()

        expected_first_line = "!    A234567   2012 10 21.40516001 46 44.001+29 13 13.27                     523\n"
        # "     A234567 HN2012 11 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"
        expected_second_line = "     A234567*HN2012 11 21.40516001 46 44.001+29 13 13.27         23.5 A      523\n"

        self.assertEqual(self.read_outputfile(),
                         expected_first_line + expected_second_line)

    def test_format_ra(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        formatted_ra, _ = mpc.format_ra_dec(10.68458, 41.26917)
        self.assertEqual(formatted_ra, "00 42 44.299")

    def test_format_dec(self):
        """
        Example based on:
         http://docs.astropy.org/en/latest/coordinates/index.html
        """
        _, formatted_dec = mpc.format_ra_dec(10.68458, 41.26917)
        self.assertEqual(formatted_dec, "+41 16 09.01")

    def test_format_ra_dec_strings(self):
        formatted_ra, formatted_dec = mpc.format_ra_dec(10.68458, 41.26917)
        self.assertEqual(formatted_ra, "00 42 44.299")
        self.assertEqual(formatted_dec, "+41 16 09.01")


class TestOSSOSComment(unittest.TestCase):
    def test_from_string(self):
        ossos_style_comment = """#O 1698860p25 O13AE2O Y   990.0 3698.0 22.79 0.08 0.096    3 % This is a comment."""
        self.assertIsInstance(mpc.OSSOSComment.from_string(ossos_style_comment), mpc.OSSOSComment)


class TestMPCReader(unittest.TestCase):
    def setUp(self):
        self.fileobj = tempfile.NamedTemporaryFile()
        self.fileobj.write("""#O   1698860p25 O13AE2O Y   990.0 3698.0 22.79 0.08 0.096    3 % This is a comment.\n""")
        self.fileobj.write("""     o3e05    C2013 06 12.37153 14 19 34.135-14 01 53.33                     568\n""")
        self.fileobj.write(
            "-    O13AE3M   2014 02 03.59026 14 28 51.800-15 18 59.40                     568 20140201_568_1 20140221 "
            "0000000000                      O 1686862p28 O13AE3M Z    22.0  112.7   UUUU %  in chip gap\n")
        self.fileobj.write(
            "     o3e01    C2014 02 24.60898 14 28 39.810-15 20 16.67         21.8 r      568 20140224_568_1 20141108 "
            "0000000000                      O 1691684p19 O13AE3M     Y   469.54 2711.47 0.06 3 21.76 0.03 %          "
            "       \n")
        self.fileobj.write(
            " O13BL3T0     C2014 08 28.46592 00 56 32.810+02 46 18.64         24.5 r      568 20131007_568_1 20141002 "
            "0000000000                      O 1736297p04 O13BL3T0 Y   696.5 4022.8 24.46 0.19 UUUU % great           "
            "       \n")
        self.fileobj.write(
            "     L3XO   * C2013 09 29.34928 00 48 17.902+03 06 58.63         24.15r      568 1656885p20 L3XO Y   "
            "288.5 4308.3 24.15 0.13 UUUU % \n")
        self.fileobj.write(
            "     L3XO   &VC2013 09 29.38812 00 48 17.721+03 06 57.86         24.44r      568 1656895p20 L3XO YV  "
            "299.3 4304.7 24.44 0.16 UUUU % \n")
        self.fileobj.write(
            "     L3XO   & C2013 09 29.43193 00 48 17.524+03 06 56.57         24.70r      568 1656906p20 L3XO Y   "
            "311.2 4298.9 24.70 0.23 UUUU % \n")
        self.fileobj.write(
            "!    L3UV      2013 10 09.51275 00 55 10.734+04 38 52.78                     568 O 1658282p30 L3UV       "
            " Z    -5.30 4353.00 0.20 0 ----- ---- % in chip gap")

        self.fileobj.flush()
        self.fileobj.seek(0)
        self._default_provisional = os.path.basename(self.fileobj.name)
        self.long_test_name = 'O13BL3T0'

    def test_old_school(self):
        """
        Make sure that we can use the MPCReader directly, in the old school way.
        :return:
        """
        r = mpc.MPCReader(self.fileobj.name, replace_provisional=True).mpc_observations
        self.assertIsInstance(r, numpy.ndarray)
        self.assertIsInstance(r[0], mpc.Observation)
        self.assertEqual(r[0].provisional_name, self._default_provisional,
                         "got >{}< expected >{}<".format(r[0].provisional_name, self._default_provisional))

    def test_use_as_reader_set_provisional(self):
        """
        Now we expect MPCReader to return an object that has a read method.
        :return:
        """
        reader = mpc.MPCReader(replace_provisional=True, provisional_name='test')
        obs = reader.read(self.fileobj)
        self.assertEqual(obs[0].provisional_name, 'test')

    def test_correctly_formatted_long_provisional_name(self):
        """
        Make sure we get the provisional name in the correct starting column.
        :return:
        """
        reader = mpc.MPCReader()
        obs = reader.read(self.fileobj)
        self.assertEqual(obs[7].provisional_name, self.long_test_name,
                         "got >{}< expected >{}<".format(obs[3].provisional_name, self.long_test_name))
        self.assertEqual(str(obs[7])[1:1 + len(self.long_test_name)], self.long_test_name,
                         "got >{}< expected >{}<".format(self.long_test_name,
                                                         str(obs[3])[1:1 + len(self.long_test_name)]))

    def test_mpc_real_to_ossos(self):
        reader = mpc.MPCReader()
        obs = reader.read(self.fileobj)
        assert isinstance(obs[2].comment, mpc.OSSOSComment)
        self.assertEqual(obs[2].comment.version, "O")
        self.assertEqual(obs[2].provisional_name, "L3XO")
        self.assertAlmostEqual(obs[2].comment.mag, 24.44, places=2)

    def test_rejection_line_writing(self):
        """
        Ensure the MPCReader is picking up the
        """
        reader = mpc.MPCReader()
        obs = reader.read(self.fileobj)
        self.assertIsNone(obs[5].comment.mag_uncertainty)


if __name__ == '__main__':
    unittest.main()

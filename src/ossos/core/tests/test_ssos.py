

__author__ = 'jjk'
from unittest import TestCase
from ossos import mpc
from astropy.time import Time


class MPCParserTest(TestCase):
    """Did this MPC line parse correctly."""

    def test_standard_mpc_line(self):

        mpc_line = "     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568"

        o = mpc.Observation.from_string(mpc_line)
        self.assertEqual(o.provisional_name, 'HL7j2')

    def test_long_provisional_mpc_line(self):

        mpc_line = "  XXXHL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568"

        o = mpc.Observation.from_string(mpc_line)
        self.assertEqual(o.provisional_name, 'XXXHL7j2')

    def test_long_provisional_mpc_line_fail(self):
        '''What happens if there are too few spaces'''

        mpc_line = "XXXHL7j2      C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568"

        o = mpc.Observation.from_string(mpc_line)
        self.assertNotEqual(o.provisional_name, 'XXXHL7j2')

    def test_mpc_high_precision(self):
        """can we get the precision on the RA correct?"""

        mpc_line = " XXHL7j2      C2013 04 03.62926 17 12 01.161+04 13 33.3          24.1 R      568"

        o = mpc.Observation.from_string(mpc_line)
        print(str(o.ra))
        self.assertEqual(str(o.ra), "17 12 01.161")

    def __test_ossos_format_mpc_line(self):

        mpc_lines_2=("     DRY001U   2013 04 09.36658 14 14 12.656-12 49 42.67         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ",
                     "     DRY001U   2013 04 09.40970 14 14 11.877-12 49 37.60         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ",
                     "     DRY001U   2013 04 09.45271 14 14 11.053-12 49 32.04         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ")

        observations = []
        for line in mpc_lines_2:
            observations.append(mpc.Observation.from_string(line))
        name = observations[0].provisional_name

        query = ssos.Query(observations,
                           search_start_date=Time('2013-02-08', scale='utc'),
                           search_end_date=Time('2013-09-01', scale='utc'))

        ssos_result = query.get()

        ssos_data = ssos.SSOSParser(name).parse(ssos_result)

        print(ssos_data.get_reading_count())



if __name__ == '__main__':
    unittest.main()

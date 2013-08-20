__author__ = 'Michele Bannister'

import unittest

from hamcrest import assert_that, equal_to, has_length, contains

from ossos import mpc
from ossos import ssos

class SSOSQueryTest(unittest.TestCase):

    def test_run_query(self):
        # this only has one DECam image
        mpc_lines=("     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
                   "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
                   "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
                   "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")
        # this one is legal for producing more OSSOS images
        mpc_lines_2=("     DRY001U   2013 04 09.36658 14 14 12.656-12 49 42.67         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ",
                     "     DRY001U   2013 04 09.40970 14 14 11.877-12 49 37.60         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ",
                     "     DRY001U   2013 04 09.45271 14 14 11.053-12 49 32.04         -1   r      568 None DRY001U Z  XXXXXX YYYYYY   UUUU % ")

        observations = []
        for line in mpc_lines_2:
            observations.append(mpc.Observation().from_string(line))

        query = ssos.SSOSQuery(observations)

        assert_that(query.format_url(), not None)
 #       assert_that(query.url, not None)

        output = query.query()


if __name__ == '__main__':
    unittest.main()

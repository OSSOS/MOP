__author__ = 'jjk'
import unittest
import sys

from hamcrest import assert_that, equal_to, has_length, contains

from astropy import coordinates
from astropy import units

from ossos import mpc
from ossos import orbfit
import os


class OrbfitTest(unittest.TestCase):

    def test_orbfit_residuals(self):
        mpc_lines=("     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
                   "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
                   "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
                   "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")

        observations = []
        for line in mpc_lines:
            observations.append(mpc.Observation.from_string(line))

        HL7j2 = orbfit.Orbfit(observations=observations)

        for observation in observations:
            HL7j2.predict(observation.date, 568)
            self.assertLess(observation.ra_residual, 0.3)
            self.assertLess(observation.dec_residual, 0.3)

    def test_null_obseravtion(self):
        mpc_lines=("!    HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
                   "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
                   "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
                   "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")

        observations  = []
        for line in mpc_lines:
            observations.append(mpc.Observation.from_string(line))

        HL7j2 = orbfit.Orbfit(observations)
        self.assertAlmostEqual(HL7j2.a, 135.75, 1)


if __name__ == '__main__':
    unittest.main()


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

      @unittest.skipIf(not os.path.exists(orbfit.LIBORBFIT),
                       "%s not found" % orbfit.LIBORBFIT)
      def test_create_from_line(self):
        mpc_lines=("     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
                   "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
                   "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
                   "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")

        observations = []
        for line in mpc_lines:
            observations.append(mpc.Observation().from_string(line))

        os.environ['ORBIT_EPHEMERIS']='/Users/jjk/MOP/config/binEphem.405'
        os.environ['ORBIT_OBSERVATORIES']='/Users/jjk/MOP/config/observatories.dat'
        HL7j2 = orbfit.Orbfit(observations=observations)
        print str(HL7j2)

        pm = '+/-'
        for observation in observations:
            HL7j2.predict(observation.date, 568)
            print "Input  : {} {} {} [ {:+4.2f} {:+4.2f} {}{:4.2f} {}{:4.2f} {:5.1f}]".format(observation.date,
                                                   observation.ra,
                                                   observation.dec,
                                                   observation.ra_residual,
                                                   observation.dec_residual,
                                                   pm,
                                                   HL7j2.dra,
                                                   pm,
                                                   HL7j2.ddec,
                                                   HL7j2.pa)
            self.assertLess(observation.ra_residual, 0.3)
            self.assertLess(observation.dec_residual, 0.3)



if __name__ == '__main__':
    unittest.main()


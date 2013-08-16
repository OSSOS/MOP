__author__ = 'jjk'
import unittest
import tempfile

from hamcrest import assert_that, equal_to, has_length, contains

from astropy import coordinates
from astropy import units

from ossos import mpc
from ossos import orbfit
import os

class OrbfitTest(unittest.TestCase):

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
        for observation in observations:
            HL7j2.predict(observation.date)
            dra = coordinates.Angle(HL7j2.coordinate.ra - observation.coordinate.ra)
            if dra.degrees > 180 :
                dra = dra - coordinates.Angle(360, unit=units.degree)
            ddec = coordinates.Angle(HL7j2.coordinate.dec - observation.coordinate.dec)
            if ddec.degrees > 180:
                dra = ddec - coordinates.Angle(360, unit=units.degree)

            print "Input  : {} {} {} [ {:+4.2f} {:+4.2f} ]".format(observation.date,
                                                   observation.ra,
                                                   observation.dec,
                                                   dra.degrees*3600.0,
                                                   ddec.degrees*3600.0)
            self.assertEquals(HL7j2.date, str(observation.date))
            self.assertLess(dra.degrees, 0.3)
            self.assertLess(ddec.degrees, 0.3)



if __name__ == '__main__':
    unittest.main()


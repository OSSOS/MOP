from __future__ import absolute_import
import unittest
from ossos import horizons
from astropy.time import Time
from astropy import units
from astropy.coordinates import SkyCoord

__author__ = 'jjk'


class HorizonsQueryTest(unittest.TestCase):

    def test_query_horizons(self):
        header = dict(MJDATE=56659.3389839,
                      MJDEND=56659.3440930)
        coords = dict(Ceres=SkyCoord(204.438071211, 1.10491971166, unit=('degree', 'degree')),
                      Vesta=SkyCoord(199.689411326, -0.761230346005, unit=('degree', 'degree')))

        query = horizons.Query('Ceres')

        start_keyword = 'MJDATE'
        stop_keyword = 'MJDEND'

        start_exposure = Time(header[start_keyword], format='mjd')
        stop_exposure = Time(header[stop_keyword], format='mjd')

        mid_exposure = Time((stop_exposure.jd + start_exposure.jd)/2.0, format='jd')

        query.start_time = start_exposure
        query.stop_time = stop_exposure
        query.step_size = 1 * units.minute

        self.assertAlmostEqual(mid_exposure.jd, Time(2456659.84154, format='jd').jd, 5)

        for target in ['Ceres', 'Vesta']:
            query.target = target
            query.predict(mid_exposure)
            self.assertLess(coords[target].separation(query.coordinate), 0.1 * units.arcsec)


if __name__ == '__main__':
    unittest.main()

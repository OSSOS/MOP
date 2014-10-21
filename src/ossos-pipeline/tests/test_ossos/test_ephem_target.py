from unittest import TestCase
from astropy import coordinates
from astropy.time import Time
from astropy import units
import cStringIO

__author__ = 'jjk'

from ossos import ephem_target


class TestEphemTarget(TestCase):

    def test_create(self):
        """Make sure we can build an object with just a name."""
        target_name = "test"
        et = ephem_target.EphemTarget(target_name)
        self.assertIn(target_name, et.name)

    def test_precision_and_wrap(self):
        """Check that the precision in the output fits within allowed space and values to round to 60s"""
        target_name = "test"
        coordinate = coordinates.ICRSCoordinates("11:59:59.9999999999 -10:00:59.9999999999",
                                                 unit=(units.hour, units.degree),
                                                 obstime=Time("2000-01-01 10:00:00", scale='utc'))
        et = ephem_target.EphemTarget(target_name)
        et.append(coordinate)
        line = et.doc.getElementsByTagName("CSV")[0].firstChild.wholeText.split('\n')[-2]
        values = line.split('|')
        self.assertEqual(len(values), 4)
        self.assertEqual(values[1], "11:59:59.99")
        self.assertEqual(values[2], "-10:00:59.9")

    def test_file_write(self):
        """Check that we are writing the file."""

        et = ephem_target.EphemTarget("")
        coordinate = coordinates.ICRSCoordinates("01:27:30.260 +13:40:30.60",
                                                 unit=(units.hour, units.degree),
                                                 obstime=Time("2014-12-28 09:59:59", scale='utc'))
        et.append(coordinate)
        coordinate = coordinates.ICRSCoordinates("01:27:29.390 +13:40:20.90",
                                                 unit=(units.hour, units.degree),
                                                 obstime=Time("2014-12-29 09:59:59", scale='utc'))
        et.append(coordinate)
        f_handle = cStringIO.StringIO()
        et.writer(f_handle)
        f_handle.seek(0)
        f_handle.close()
from unittest import TestCase
from astropy import coordinates
from astropy.time import Time
from astropy import units
import io

__author__ = 'jjk'

from ossos import ephem_target

example_file="""<?xml version="1.0" ?>
<!DOCTYPE ASTRO
  SYSTEM 'http://vizier.u-strasbg.fr/xml/astrores.dtd'>
  <ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
    <TABLE ID="Table">
      <NAME>Ephemeris</NAME>
      <TITLE>Ephemeris for CFHT QSO target Test</TITLE>
      <!--Definition of each field-->
      <FIELD datatype="A" format="YYYY-MM-DD hh:mm:ss" name="DATE_UTC" width="19">
        <DESCRIPTION>UTC Date</DESCRIPTION>
      </FIELD>
      <FIELD datatype="A" format="RAh:RAm:RAs" name="RA_J2000" unit="h" width="11">
        <DESCRIPTION>Right ascension of target</DESCRIPTION>
      </FIELD>
      <FIELD datatype="A" format="DEd:DEm:DEs" name="DEC_J2000" unit="deg" width="11">
        <DESCRIPTION>Declination of target</DESCRIPTION>
      </FIELD>
      <!--Data table-->
      <DATA>
        <CSV colsep="|" headlines="4">
<![CDATA[
DATE_UTC           |RA_J2000   |DEC_J2000  |
YYYY-MM-DD hh:mm:ss|RAh:RAm:RAs|DEd:DEm:DEs|
0123456789012345678|01234567890|01234567890|
-------------------|-----------|-----------|
2014-12-28 09:59:59|01:27:30.26|+13:40:30.6|
]]>        </CSV>
      </DATA>
    </TABLE>
  </ASTRO>
"""

class TestEphemTarget(TestCase):

    def test_create(self):
        """Make sure we can build an object with just a name."""
        target_name = "test"
        et = ephem_target.EphemTarget(target_name)
        self.assertIn(target_name, et.name)

    def test_precision_and_wrap(self):
        """Check that the precision in the output fits within allowed space and values to round to 60s"""
        target_name = "test"
        coordinate = coordinates.SkyCoord("11:59:59.9999999999 -10:00:59.9999999999",
                                          unit=(units.hour, units.degree),
                                          obstime=Time("2000-01-01 10:00:00", scale='utc'))
        et = ephem_target.EphemTarget(target_name)
        et.append(coordinate)
        line = et.doc.getElementsByTagName("CSV")[0].firstChild.wholeText.split('\n')[-2]
        values = line.split('|')
        self.assertEqual(len(values), 4)
        self.assertEqual(values[1], "12:00:00.00")
        self.assertEqual(values[2], "-10:01:00.0")

    def test_file_write(self):
        """Check that we are writing the file."""

        et = ephem_target.EphemTarget("Test")
        coordinate = coordinates.SkyCoord("01:27:30.26 +13:40:30.60",
                                          unit=(units.hour, units.degree),
                                          obstime=Time("2014-12-28 09:59:59", scale='utc'))
        et.append(coordinate)
        # coordinate = coordinates.SkyCoord("01:27:29.39 +13:40:20.90",
        #                                   unit=(units.hour, units.degree),
        #                                   obstime=Time("2014-12-29 09:59:59", scale='utc'))
        # et.append(coordinate)
        print(et.coordinates)
        f_handle = io.StringIO()
        et.writer(f_handle)
        f_handle.seek(0)
        self.assertEqual(f_handle.read(), example_file)
        f_handle.close()

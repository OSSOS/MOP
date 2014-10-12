#!/usr/bin/env python
"""Create an ephemeris file for a KBO based on the .abg orbit file.
   output is in CFHT format"""

import logging
import argparse

import datetime
from astropy import units, coordinates, time

from ossos import mpc
from ossos import orbfit
from ossos.cameras import Camera


class EphemTarget:

    ASTRO_FORMAT_HEADER="""<?xml version = "1.0"?>
<!DOCTYPE ASTRO SYSTEM "http://vizier.u-strasbg.fr/xml/astrores.dtd"> <ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
<TABLE ID="Table">
<NAME>Ephemeris</NAME>
<TITLE>Ephemeris for CFHT QSO</TITLE>
<!-- Definition of each field -->
<FIELD name="DATE_UTC" datatype="A" width="19" format="YYYY-MM-DD hh:mm:ss">
<DESCRIPTION>UTC Date</DESCRIPTION></FIELD>
<FIELD name="RA_J2000" datatype="A" width="11" unit="h" format="RAh:RAm:RAs">  <DESCRIPTION>Right ascension of target</DESCRIPTION>  </FIELD>
<FIELD name="DEC_J2000" datatype="A" width="11" unit="deg" format="DEd:DEm:DEs">  <DESCRIPTION>Declination of target</DESCRIPTION>  </FIELD>
<!-- Data table -->
<DATA><CSV headlines="4" colsep="|">
<![CDATA[
DATE_UTC           |RA_J2000   |DEC_J2000  |
YYYY-MM-DD hh:mm:ss|hh:mm:ss.ss|+dd:mm:ss.s|
1234567890123456789|12345678901|12345678901|
-------------------|-----------|-----------|
"""
    ASTRO_FORMAT_FOOTER="""]]></CSV></DATA>
</TABLE>
</ASTRO>
"""

    def __init__(self, name):
        """
        create an ephmeris target, either with a 'orbfit' object or some mean rate of motion.

        :param name: a string containing the name of the target.
        """
        self.name = str(name)
        self.coordinates = []

    def save(self, filename=None):
        """
        Save the ephemeris to a CFHT ETarget formated file.

        :param filename: name of file to write to, defaults to "ET"+name+".xml"
        :return: result of close()
        """
        if filename is None:
            # SG would like us to ALWAYS have the block name in the OBJECT keyword for a given observation.
            filename = "ET_" + self.name + ".xml"  # FIXME: this method could be handled more smoothly
        et_file = open(filename, 'w')
        et_file.write(self.ASTRO_FORMAT_HEADER)
        for coordinate in self.coordinates:
            assert isinstance(coordinate, coordinates.ICRSCoordinates)
            sra = coordinate.ra.format(units.hour, sep=':')
            sdec = coordinate.dec.format(units.degree, sep=':')
            sdate = str(coordinate.obstime.replicate(format('iso')))
            et_file.write("%19s|%11s|%11s|\n" % (sdate[0:19], sra[0:11], sdec[0:11]))
        et_file.write(self.ASTRO_FORMAT_FOOTER)
        return et_file.close()


if __name__ == '__main__':
    logger = logging.getLogger()
    parser = argparse.ArgumentParser()
    parser.add_argument('mpc_files', nargs='+', help='mpc_file to base ephemeris from.')
    parser.add_argument('--verbose', '-v', action='store_true', default=None, help='verbose feedback')
    parser.add_argument('--start', '-s', default=None, help='Date as YYYY/MM/DD, default is current date')
    parser.add_argument('--range', '-r', default=30, help='Length of ephemeris is days', type=int)
    parser.add_argument('--ccd', '-c', default=22, help='Offset so target is on this CCD (0 is the first CCD)')
    parser.add_argument('--geometry', '-g', default="MEGACAM_36",
                        help='camera geometry (see ossos.cameras for options); default is MEGACAM_36')
    parser.add_argument('--dra', default=0, help='Additional RA offset (arcmin)')
    parser.add_argument('--ddec', default=0, help='Additional DECA offset (arcmin)')

    opt = parser.parse_args()
    if opt.verbose:
        logger.setLevel(logging.INFO)

    start_date = (opt.start is not None and time.Time(opt.start, scale='utc')) or \
                 time.Time(datetime.datetime.utcnow().isoformat(), scale='utc')

    offset = Camera.geometry[opt.geometry]

    ## build orbit instance for object
    for mpc_file in opt.mpc_files:
        obs = []
        for line in open(mpc_file, 'r'):
            if not line.startswith('#'):
                ob = mpc.Observation.from_string(line)
                ob.null_observation = False
                obs.append(ob)
        orbit = orbfit.Orbfit(obs)

        et = EphemTarget(orbit.name)
        for day in range(opt.range):
            orbit.predict(time.Time(start_date.jd+day, scale='utc', format='jd'))
            if opt.ccd:
                orbit.coordinate.ra = orbit.coordinate.ra + coordinates.RA(offset[int(opt.ccd)]["ra"], units.degree)
                orbit.coordinate.dec = orbit.coordinate.dec + coordinates.Dec(offset[int(opt.ccd)]["dec"], units.degree)
            if opt.dra:
                orbit.coordinate.ra = orbit.coordinate.ra + coordinates.RA(float(opt.dra), units.degree)
            if opt.ddec:
                orbit.coordinate.dec = orbit.coordinate.dec + coordinates.Dec(float(opt.ddec), units.degree)
            et.coordinates.append(orbit.coordinate)
        et.save()


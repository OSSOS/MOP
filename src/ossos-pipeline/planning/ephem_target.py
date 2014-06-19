#!/usr/bin/env python
"""Create an ephemeris file for a KBO based on the .abg orbit file.
   output is in CFHT format"""


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
-------------------|-----------|-----------|"""
ASTRO_FORMAT_FOOTER="""]]></CSV></DATA>
</TABLE>
</ASTRO>"""

#DIR_DB = "/Users/dbase/TNOdb/dbase"
#orbb=DIR_DB+"/data/orbb"
DIR_DB = "./"
orbb = DIR_DB

from astropy import units 


geometry={"MegaPrime":[
        {"ra": -0.46, "dec": -0.38, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.35, "dec": -0.38, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.23, "dec": -0.38, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.12, "dec": -0.38, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.00, "dec": -0.38, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.11, "dec": -0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.23, "dec": -0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.35, "dec": -0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.46, "dec": -0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.47, "dec": -0.12, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.35, "dec": -0.12, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.23, "dec": -0.12, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.12, "dec": -0.12, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.00, "dec": -0.12, "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.12, "dec": -0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.23, "dec": -0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.35, "dec": -0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.46, "dec": -0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.47, "dec": 0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.35, "dec": 0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.23, "dec": 0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.12, "dec": 0.12,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.00, "dec": 0.12,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.12, "dec": 0.12,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.23, "dec": 0.12,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.35, "dec": 0.12,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.47, "dec": 0.12,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.46, "dec": 0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.35, "dec": 0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.23, "dec": 0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": -0.12, "dec": 0.38,  "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.00, "dec": 0.38,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.12, "dec": 0.38,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.23, "dec": 0.38,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.35, "dec": 0.38,   "dra": 0.1052, "ddec": 0.2344 },
        {"ra": 0.47, "dec": 0.38,   "dra": 0.1052, "ddec": 0.2344 }],
       "MegaCam":[
{'ra': -0.162500, 'dec':-0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':-0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':-0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':-0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':0.000000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.162500, 'dec':0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':-0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':-0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':-0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':-0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':0.000000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': -0.051389, 'dec':0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':-0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':-0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':-0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':-0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':0.000000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.051389, 'dec':0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':-0.189444 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':-0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':-0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':-0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':0.000000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':0.047500 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':0.095000 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':0.141944 , 'dra':0.102222 , 'ddec':0.045333},
{'ra': 0.162500, 'dec':0.189444 , 'dra':0.102222 , 'ddec':0.045333}]}

from ossos import mpc
from ossos import orbfit
import logging
import argparse
import ephem

logger = logging.getLogger()
parser = argparse.ArgumentParser()
parser.add_argument('mpc_files', nargs='+', help='mpc_file to base ephemeris from.')
parser.add_argument('--verbose','-v', action='store_true', default=None, help='verbose feedback')
parser.add_argument('--start', '-s', default=None, help='Date as YYYY/MM/DD, default is curernt date')
parser.add_argument('--range', '-r', default=30, help='Length of ephemeris is days', type=int)
parser.add_argument('--ccd', '-c', default=22, help='Offset so target is on this CCD')
parser.add_argument('--geometry', '-g', default="MegaPrime", help='camera geometry (MegaCam or MegaPrime)')
parser.add_argument('--dra', default=0, help='Additional RA offset (arcmin)')
parser.add_argument('--ddec', default=0, help='Additional DECA offset (arcmin)')



opt = parser.parse_args()
if opt.verbose:
    logger.setLevel(logging.INFO)

julian_date = ephem.julian_date()
if opt.start is not None:
    try:
        julian_date = ephem.julian_date(opt.start)
    except:
        logger.error(opt.start)
        parser.error("Don't recognize date format use YYYY/MM/DD")

jd0 = ephem.julian_date(0)
offset = geometry[opt.geometry]

count = 0
print ASTRO_FORMAT_HEADER
for mpc_file in opt.mpc_files:
    obs = []
    for line in open(mpc_file, 'r'):
        ob = mpc.Observation.from_string(line)
        ob.null_observation = False
        obs.append(ob)
    orbit = orbfit.Orbfit(obs)
    for day in range(opt.range):
        count = count+1
        smjd = julian_date+day
        orbit.predict(smjd)
        sra = orbit.coordinate.ra.format(units.hour, sep=':')
        sdec = orbit.coordinate.dec.format(units.degree, sep=':')
        sdate = str(ephem.date(smjd-jd0))
        sdate = sdate.replace("/","-")
        print "%19s|%11s|%11s|" % ( sdate[0:19],sra[0:11],sdec[0:11])


print ASTRO_FORMAT_FOOTER



        
        

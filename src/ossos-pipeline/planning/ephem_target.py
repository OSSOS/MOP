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
	



import logging
import optparse, math
import ephem, time, orbfit

logger=logging.getLogger()
parser=optparse.OptionParser()
parser.add_option('--verbose','-v',action='store_true',default=None,
                  help='verbose feedback')
parser.add_option('--start','-s',action='store',default=None,help='Date as YYYY/MM/DD, default is curernt date')
parser.add_option('--range','-r',action='store',default=30,help='Length of ephemeris is days',type=int)
parser.add_option('--ccd','-c',action='store',default=22,help='Offset so target is on this CCD')
parser.add_option('--geometry','-g',action='store',default="MegaPrime",help='camera geometry (MegaCam or MegaPrime)')
parser.add_option('--dra',action='store',default=0,help='Additional RA offset (arcmin)')
parser.add_option('--ddec',action='store',default=0,help='Additional DECA offset (arcmin)')




(opt, abgs) = parser.parse_args()
if opt.verbose:
    logger.setLevel(logging.INFO)

if opt.start is None:
    julian_date = ephem.julian_date()
else:
    try:
        julian_date = ephem.julian_date(opt.start)
    except:
        logger.error("Don't recognize date format use YYYY/MM/DD")
        logger.error(opt.start)

jd_0 = ephem.julian_date(ephem.date(0))

offset=geometry[opt.geometry]

count=0
print ASTRO_FORMAT_HEADER
for abg in abgs:
    obj=abg
    abg = orbb+"/"+abg+".abg"
    for day in range(opt.range):
        count=count+1
        sdate = str(ephem.date(julian_date+day-jd_0))
        (ra,dec,dra,ddec,dang)=orbfit.predict(abg,julian_date+day,568)
        ra=ephem.hours(math.radians(ra)+math.radians(offset[int(opt.ccd)]['ra'])+math.radians(float(opt.dra)/60.0))
        dec=ephem.degrees(math.radians(dec)+math.radians(offset[int(opt.ccd)]['dec'])+math.radians(float(opt.ddec)/60.0))
        sdate = str(ephem.date(julian_date+day-jd_0))
        sdate=sdate.replace("/","-")
        sra   = str(ra)
        sdec  = str(dec)
        if math.fabs(math.degrees(dec))< 10 :
            sdec = "0"+sdec
        if dec > 0 :
            sdec = "+"+sdec
        print "%19s|%11s|%11s|" % ( sdate[0:19],sra[0:11],sdec[0:11])
        #print "%10d %15s %11s %11s 2000 0 0 0 0 0 0 0 0 0 0 %8.2f %8.2f %8.2f  " % (count,obj,sra[0:11],sdec[0:11],dra,ddec,dang)
        

print ASTRO_FORMAT_FOOTER



        
        

from math import sqrt
import operator
import math
import sys
import os
import string

from matplotlib.patches import Rectangle
from matplotlib.patches import Ellipse
from matplotlib import rcParams
import numpy as np
from matplotlib.pyplot import figure, savefig
from astropy.io import votable
import ephem
import Polygon
import Polygon.IO

from . import mpcread
from . import usnoB1
from . import megacam
from ossos import storage
from ossos import orbfit
from ossos import mpc


USER = '/Users/michele/'

def plot_line(axes, fname, ltype):
    """plot the ecliptic plane line on the given axes."""
    x = np.genfromtxt(fname, unpack=True)
    axes.plot(x[0], x[1], ltype)


MPCORB_FILE = os.path.join(os.getenv('HOME', USER), 'MPCORB-Distant.DAT')
L7MODEL = 'vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt'
# L7MODEL = '/Users/jjk/Dropbox/Research/KuiperBelt'
# L7MODEL = '/tmp/vospace/OSSOS/CFEPS/L7SyntheticModel-v09.txt'
# REAL_KBO_AST_DIR = '/Users/jjk/Dropbox/dbaseclone/ast/'
# REAL_KBO_AST_DIR = '/Users/jjk/Dropbox/Research/KuiperBelt/dbase/TNOdb/dbase/data/ast/'  # this is CFEPS
REAL_KBO_AST_DIR = USER + 'Dropbox/OSSOS/measure3/ossin/'

PLOT_FIELD_EPOCH = 'Feb15'  # Oct14.00 ==> '0' days since the New Moon on Oct14
#TODO the .00 is appended when this variable is used as a keyword that needs that .00 this is bad.
DISCOVERY_NEW_MOON = 'Nov13'  # this is the date that the RA/DEC in blocks corresponds to.


PLOT_USNO_STARS = False
PLOT_MEGACAM_ARCHIVE_FIELDS = True
PLOT_SYNTHETIC_KBOS = True
PLOT_SYNTHETIC_KBO_TRAILS = False
PLOT_REAL_KBOS = False and os.access(REAL_KBO_AST_DIR,os.F_OK)
PLOT_FIELD_LAYOUT = True

PLOT_MPCORB = False and os.access(MPCORB_FILE, os.F_OK)

LABEL_FIELDS = True
LABEL_PLANETS = True
# # EmulateApJ columnwidth=245.26 pts
fig_width_pt = 246.0 * 3.0
inches_per_pt = 1.0 / 72.27
golden_mean = (sqrt(5.) - 1.0) / 2.0
fig_width = fig_width_pt * inches_per_pt
fig_height = fig_width * golden_mean * 1.5
fig_size = [fig_width, fig_height]

params = {'backend': 'pdf',
          'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 10,
          'xtick.labelsize': 10,
          'ytick.labelsize': 10,
          'text.usetex': False,
          'font.serif': 'Times',
          'font.family': 'serif',
          'image.aspect': 'equal',
          'figure.subplot.left': 0.2,
          'figure.subplot.bottom': 0.15,
          'figure.figsize': fig_size}

rcParams.update(params)

etHeader = """<?xml version = "1.0"?>
<!DOCTYPE ASTRO SYSTEM "http://vizier.u-strasbg.fr/xml/astrores.dtd">
<ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
  <TABLE ID="Table">
    <NAME>Ephemeris</NAME>
    <TITLE>Ephemeris for CFHT QSO</TITLE>
    <!-- Definition of each field -->
    <FIELD name="DATE_UTC"  datatype="A" width="19" format="YYYY-MM-DD hh:mm:ss">
        <DESCRIPTION>UTC Date</DESCRIPTION>
    </FIELD>
    <FIELD name="RA_J2000"  datatype="A" width="11" unit="h"   format="RAh:RAm:RAs">
        <DESCRIPTION>Right ascension of target</DESCRIPTION>
    </FIELD>
    <FIELD name="DEC_J2000" datatype="A" width="11" unit="deg" format="DEd:DEm:DEs">
        <DESCRIPTION>Declination of target</DESCRIPTION>
    </FIELD>
    <!-- Data table -->
<DATA><CSV headlines="4" colsep="|">
<![CDATA[
DATE_UTC           |RA_J2000   |DEC_J2000  |
YYYY-MM-DD hh:mm:ss|hh:mm:ss.ss|+dd:mm:ss.s|
1234567890123456789|12345678901|12345678901|
-------------------|-----------|-----------|
"""

header = """<?xml version = "1.0"?>
<!DOCTYPE ASTRO SYSTEM "http://vizier.u-strasbg.fr/xml/astrores.dtd">
<ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
<TABLE ID="Table">
<NAME>Fixed Targets</NAME>
<TITLE>Fixed Targets for CFHT QSO</TITLE>
<!-- Definition of each field -->
<FIELD name="NAME" datatype="A" width="20">
   <DESCRIPTION>Name of target</DESCRIPTION>
</FIELD>
<FIELD name="RA" ref="" datatype="A" width="11" unit="&quot;h:m:s&quot;">
   <DESCRIPTION>Right ascension of target</DESCRIPTION>
</FIELD>
<FIELD name="DEC" ref="" datatype="A" width="11" unit="&quot;d:m:s&quot;">
   <DESCRIPTION>Declination of target</DESCRIPTION>
</FIELD>
<FIELD name="EPOCH" datatype="F" width="6">
    <DESCRIPTION>Epoch of coordinates</DESCRIPTION>
</FIELD>
<FIELD name="POINT" datatype="A" width="5">
<DESCRIPTION>Pointing name</DESCRIPTION>
</FIELD>
<!-- Data table -->
<DATA><CSV headlines="4" colsep="|"><![CDATA[
NAME                |RA         |DEC        |EPOCH |POINT|
                    |hh:mm:ss.ss|+dd:mm:ss.s|      |     |
12345678901234567890|12345678901|12345678901|123456|12345|
--------------------|-----------|-----------|------|-----|
"""

# spring13
# blocks = {#'13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},  # E+0+0: image 1616681, ccd21 on April 9
#'13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8
#'14AN': {'RA': "15:30:00.00", "DEC": "-11:00:00.0"},
#'14AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"},
#'14AS': {'RA': "15:12:00.00", "DEC": "-17:40:00.0"}}


#fall13
blocks = {'13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},  # ,
          '13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},  # E+0+0: image 1616681, ccd21 on April 9
          '13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8
          '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"},
          '14AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"}
          }

## this position is derived from the +0+0 field for 13B observed on November 1st 2013
blocks = {'14BH': {'RA': "01:28:32.32", "DEC": "+12:51:06.10"}}

#fall 2015
# blocks = {'15BD': {'RA': "03:15:00.00", "DEC": "+16:30:00.00"}}

# spring14

# blocks = {'15AP': {'RA': "13:30:00", "DEC": "-07:00:00"}}

newMoons = {
#    'Feb13': "2013/02/10 10:00:00",
#    'Mar13': "2013/03/11 10:00:00",
#    'Apr13': "2013/04/10 10:00:00",
#    'May13': "2013/05/09 10:00:00",
#    'Jun13': "2013/06/08 10:00:00",
#    'Jul13': "2013/07/08 10:00:00",
#    'Aug13': "2013/08/06 10:00:00",
#    'Sep13': '2013/09/05 10:00:00',
#    'Oct13': '2013/10/04 10:00:00',
'Nov13': '2013/11/03 10:00:00',
#    'Dec13': '2013/12/02 10:00:00',
#    'Jan14': '2014/01/01 10:00:00',
#    'Feb14': '2014/01/31 10:00:00',
#    'Mar14': '2014/03/28 10:00:00',
# 'Apr14': '2014/04/01 10:00:00',
#    'May14': '2014/05/28 10:00:00',
#    'Jun14': '2014/06/26 10:00:00',
#    'Jul14': '2014/07/26 10:00:00',
#    'Aug14': "2014/08/25 10:00:00",
#    'Sep14': '2014/09/24 10:00:00',
#     'Oct14': '2014/10/23 10:00:00',
'Nov14': '2014/11/22 10:00:00',
#    'Dec14': '2014/12/22 10:00:00',
#    'Jan15': '2015/01/20 10:00:00',
    'Feb15': '2015/02/18 10:00:00',
    'Mar15': '2015/03/19 10:00:00',
    # 'Apr15': '2015/04/18 10:00:00',
    # 'May15': '2015/05/17 10:00:00',
    # 'Jun15': '2015/06/16 10:00:00',
    # 'Jul15': '2015/07/15 10:00:00',
    # 'Aug15': '2015/08/14 10:00:00',
    # 'Sep15': '2015/09/12 10:00:00',
    # 'Oct15': '2015/10/12 10:00:00',
    # 'Nov15': '2015/11/11 10:00:00',
    # 'Dec15': '2015/12/11 10:00:00',
    # 'Jan16': '2016/01/09 10:00:00',
    # 'Feb16': '2016/03/08 10:00:00',
    # 'Mar16': '2016/03/09 10:00:00',
    # 'Apr16': '2016/04/08 10:00:00',
    # 'May16': '2016/05/07 10:00:00',
    # 'Jun16': '2016/06/05 10:00:00',
    # 'Jul16': '2016/07/05 10:00:00',
    # 'Aug16': '2016/08/03 10:00:00',
    # 'Sep16': '2016/09/01 10:00:00',
    # 'Oct16': '2016/10/01 10:00:00',
    # 'Nov16': '2016/11/01 10:00:00',
    # 'Dec16': '2016/12/01 10:00:00',
    # 'Jan17': '2017/01/01 10:00:00',
    # 'Feb17': '2017/02/01 10:00:00',
}



xgrid = {'2014': [-3, -2, -1, 0, 1, 2, 3],
         '2014r': [-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5],
         'astr': [1, 2, 3, 4]}
# 2013
#       'astr':[-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5]}

ygrid = {'2014': [-1, 0, 1],
         'astr': [-2.5, -1.5, -0.5, 0.5, 1.5],
         '2014r': [-1.5, -0.5, 0.5, 1.5]}
# 2013
#       'astr':  [-2.2,-1.2, -0.2, 0.8, 1.8],

block_centre = ephem.Ecliptic(0, 0)
field_centre = ephem.Ecliptic(0, 0)

## foffset is the offset (in RA and DEC) between fields
field_offset = math.radians(1.00)
# which year, for the x/y offset grids, are we using.
year = '2014'

# the 'astr' year indicates astrometric grid,  this should have more overlap so reduce the field_offset for those.
if year == 'astr':
    field_offset *= 0.75

## camera_dimen is the size of the field.
camera_dimen = 0.98

years = {"2014": {"ra_off": ephem.hours("00:00:00"),
                  "dec_off": ephem.hours("00:00:00"),
                  "fill": False,
                  "facecolor": 'k',
                  "alpha": 0.5,
                  "color": 'b'},
         "2014r": {"ra_off": ephem.hours("00:05:00"),
                   "dec_off": ephem.degrees("00:18:00"),
                   "alpha": 0.5,
                   "fill": False,
                   "facecolor": 'k',
                   "color": 'g'},
         'astr': {"ra_off": ephem.hours("00:00:00"),
                  "dec_off": ephem.degrees("-00:12:00"),
                  "alpha": 0.05,
                  "fill": True,
                  'facecolor': 'k',
                  "color": 'none'},
         }

# coverage is going to contain the polygons that describe our individual fields.
coverage = []

# ras/decs will hold the centers of the search fields, in degrees.
ras = []
decs = []

fix = open('Fall14.xml', 'w')
fix.write(header)

# build the pointings that will be used for the discovery fields
# This is a bit tricky as we keep the heliocentric longitude constant while moving in RA
# and get latitude coverage by changing the DEC.
# Compute the coverage of the discovery fields.
field_names = []
field_polygon = None

for block in list(blocks.keys()):
    rac = ephem.hours(blocks[block]["RA"]) + years[year]["ra_off"]
    decc = ephem.degrees(blocks[block]["DEC"]) + years[year]["dec_off"]
    width = field_offset / math.cos(decc)
    block_centre.from_radec(rac, decc)
    block_centre.set(block_centre.lon + xgrid[year][0] * width, block_centre.lat)
    field_centre.set(block_centre.lon, block_centre.lat)
    for dx in xgrid[year]:
        (rac, decc) = field_centre.to_radec()
        for dy in ygrid[year]:
            ddec = dy * field_offset
            dec = math.degrees(decc + ddec)
            ra = math.degrees(rac)
            field_name = "%s%+d%+d" % (block, dx, dy)
            field_names.append(field_name)
            decs.append(np.radians(dec))
            ras.append(np.radians(ra))
            xcen = ra
            ycen = dec
            fix.write('%20s|%10s|%10s|2000.0|    1|\n' % (field_name,
                                                          ephem.hours(math.radians(ra)),
                                                          ephem.degrees(math.radians(dec))))
            dimen = camera_dimen
            this_point_polygon = Polygon.Polygon((
                (xcen - dimen / 2.0, ycen - dimen / 2.0),
                (xcen - dimen / 2.0, ycen + dimen / 2.0),
                (xcen + dimen / 2.0, ycen + dimen / 2.0),
                (xcen + dimen / 2.0, ycen - dimen / 2.0),
                (xcen - dimen / 2.0, ycen - dimen / 2.0)))
            field_polygon = field_polygon is None and this_point_polygon or this_point_polygon | field_polygon

        rac += field_offset / math.cos(decc)
        for i in range(3):
            field_centre.from_radec(rac, decc)
            field_centre.set(field_centre.lon, block_centre.lat)
            (ttt, decc) = field_centre.to_radec()

fix.write("""]]</CSV></DATA>
</TABLE>
</ASTRO>
""")
fix.close()

field_polygon.simplify()
print("Field Area: {} degrees squared".format(field_polygon.area()))
print("Field Centre: {} ".format(field_polygon.center()))

(ra_min, ra_max, dec_min, dec_max) = field_polygon.boundingBox()

if year == "astr":
    ## we don't do trailing for the astr ometric grid, or make any plots...
    sys.exit(0)

ras = np.array(ras)
decs = np.array(decs)
ra_cen = math.degrees(ras.mean())
dec_cen = math.degrees(decs.mean())
#ra_cen = 15.0
#dec_cen = 5.0
#width = 245 - 205
#height = -8 + 25
#ra_cen = 180.0
#dec_cen = 0.0
#width = 360
#height = 70
#ra_cen = 45.0
#dec_cen = 17.5
width = 10
height = 10

## lets make a plot of the field selections.
fig = figure()
ax = fig.add_subplot(111)


ax.set_xlim(ra_cen+width/2.0, ra_cen-width/2.0)
ax.set_ylim(dec_cen - height/2.0, dec_cen + height/2.0)
#ax.set_ylim(-30,30)
ax.set_xlabel('RA (deg)')
ax.set_ylabel('DE (deg)')

ax.grid()


## plot the galactic plane line ..
plot_line(ax, 'eplane.radec', 'b-')
plot_line(ax, 'gplane.radec', 'g-')


# Plot the invariable plane: values from DE405, table 5, Souami and Souchay 2012
# http://www.aanda.org/articles/aa/pdf/2012/07/aa19011-12.pdf
# Ecliptic J2000
invar_i = ephem.degrees('1.57870566')
invar_Om = ephem.degrees('107.58228062')
# print invar_i, invar_Om
invar = []
for n in range(1, 360):
    ec = ephem.Ecliptic(ephem.degrees(str(n)) + invar_Om, invar_i)  # lon then lat
    eq = ephem.Equatorial(ec)
    invar.append(eq)
    # print n, ec.lat, ec.lon, eq.ra, eq.dec, math.degrees(eq.ra), math.degrees(eq.dec)
ax.plot([math.degrees(eq.ra) for eq in invar], [math.degrees(eq.dec) for eq in invar], 'k-')



## build a list of Synthetic KBOs that will be in the discovery fields.
print("LOADING SYNTHETIC MODEL KBOS FROM: {}".format(L7MODEL))
ra = []
dec = []
kbos = []
lines = storage.open_vos_or_local(L7MODEL).read().split('\n')

# Look for synthetic KBOs that are in the field on this date.
discovery_date = ephem.date(newMoons[DISCOVERY_NEW_MOON])
plot_date = ephem.date(newMoons[PLOT_FIELD_EPOCH])

for line in lines:
    if len(line) == 0 or line[0] == '#':  # skip initial column descriptors and the final blank line
        continue
    kbo = ephem.EllipticalBody()
    values = line.split()
    kbo._a = float(values[0])
    kbo._e = float(values[1])
    kbo._inc = float(values[2])
    kbo._Om = float(values[3])
    kbo._om = float(values[4])
    kbo._M = float(values[5])
    kbo._H = float(values[6])
    epoch = ephem.date(2453157.50000 - ephem.julian_date(0))
    kbo._epoch_M = epoch
    kbo._epoch = epoch
    kbo.name = values[8]
    kbo.compute(discovery_date)

    kbo_ra = math.degrees(kbo.ra)
    kbo_dec = math.degrees(kbo.dec)
    ## keep a list of KBOs that are in the discovery pointings
    if field_polygon.isInside(math.degrees(float(kbo.ra)), math.degrees(float(kbo.dec))):
        ### only keep objects that are brighter than limit
        if kbo.mag < 25.0:  # and (kbo.name == 'classical' and values[9] == 'm'): # and values[10] == 'k'):
            kbos.append(kbo)
            if PLOT_SYNTHETIC_KBOS:
                kbo.compute(plot_date)
                ax.scatter(math.degrees(float(kbo.ra)), math.degrees(float(kbo.dec)), c='k', marker='o', s=2, alpha=0.8)

print("{} KBOs found in coverage on {}".format(len(kbos), discovery_date))
## Now we work out how far to move the fields at different lunations
seps = {}
dates = {}

## build a list of dates that we will need field locations for.
for month in newMoons:
    for night in range(-8, 9):
        epoch = "%s.%s" % (month, string.zfill(night, 2))
        dates[epoch] = ephem.date(ephem.date(newMoons[month]) + night)
        seps[epoch] = {'dra': 0, 'ddec': 0}

print("COMPUTING NOMINAL MOTIONS USING SYNTHETIC MODEL KBOS FROM: {}".format(L7MODEL))

## Compute the typical motion of KBOs that were in the discovery field.
for kbo in kbos:
    kbo.compute(discovery_date)
    ra = kbo.ra
    dec = kbo.dec
    for epoch in dates:
        date = dates[epoch]
        kbo.compute(date)
        seps[epoch]['dra'] += kbo.ra - ra
        seps[epoch]['ddec'] += kbo.dec - dec

## plot source locations at the start
## middle and end of semester
if PLOT_SYNTHETIC_KBO_TRAILS:
    print("PLOTTING TRAILS USING SYNTHETIC MODEL KBOS FROM: {}".format(L7MODEL))
    colours = {'Sep14': 'g', 'Oct14': 'b', 'Nov14': 'r'}
    alpha = {'Sep14': 0.3, 'Oct14': 0.7, 'Nov14': 0.3}
    zorder = {'Sep14': 1, 'Oct14': 5, 'Nov14': 2}
    for month in ['Sep14', 'Oct14', 'Nov14']:
        ra = []
        dec = []
        date = ephem.date(newMoons[month])
        for kbo in kbos:
            kbo.compute(date)
            ra.append(math.degrees(kbo.ra))
            dec.append(math.degrees(kbo.dec))
            ax.plot(ra, dec, colours[month] + '.', alpha=alpha[month], zorder=zorder[month])

# compute the mean of the separation between the location at discovery and the location at epoch.
for month in seps:
    seps[month]['dra'] /= float(len(kbos))
    seps[month]['ddec'] /= float(len(kbos))


sorted_epochs = sorted(iter(dates.items()), key=operator.itemgetter(1))

print("CREATING ET XML FILES USING BASE POINTING AND NOMINAL MOTION RATES")
for idx in range(len(ras)):
    name = field_names[idx]
    f = file('%s.xml' % name, 'w')
    f.write(etHeader)
    for epoch in sorted_epochs:
        epoch = epoch[0]
        date = dates[epoch]
        ra = ras[idx] + seps[epoch]['dra']
        dec = decs[idx] + seps[epoch]['ddec']
        tdate = date.tuple()
        zf = string.zfill
        sdate = "%4s-%2s-%2s %2s:%2s:%2s" % (
            tdate[0], zf(tdate[1], 2), zf(tdate[2], 2), zf(tdate[3], 2), zf(tdate[4], 2), zf(int(tdate[5]), 2))
        f.write('%19s|%11s|%11s|\n' % (sdate, ephem.hours(ra), ephem.degrees(dec)))
        if epoch == PLOT_FIELD_EPOCH+".00" and PLOT_FIELD_LAYOUT:
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - dimen / 2.0, math.degrees(dec) - dimen / 2.0),
                                    height=camera_dimen,
                                    width=camera_dimen,
                                    edgecolor='b',
                                    lw=0.5, fill=True, alpha=0.3))
            if LABEL_FIELDS :
                ax.text(math.degrees(ra), math.degrees(dec),
                        name,
                        horizontalalignment='center',
                        verticalalignment='center',
                        zorder=10,
                        fontdict={'size': 4, 'color': 'darkblue'})


    f.write("""]]</CSV></DATA>\n</TABLE>\n</ASTRO>\n""")
    f.close()


if PLOT_USNO_STARS:
    print("PLOTTING LOCATIONS NEARBY BRIGHT USNO B1 STARS")
    for ra in range(int(ra_cen - width/2.0),int(ra_cen + width/2.), 10):
        for dec in range(int(dec_cen - height/2.0),int(dec_cen + height/2.), 10):
            file_name = USER + "new_usno/usno{:5.2f}{:5.2f}.xml".format(ra, dec).replace(" ", "")
            print(file_name)
            file_name = file_name.replace(" ","")
	    if not os.access(file_name, os.R_OK):
                usno = usnoB1.TAPQuery(ra, dec, 10.0, 10.0)
		fobj=open(file_name,'w')
	        fobj.write(usno.read())
                fobj.close()
            try:
               t = votable.parse(open(file_name,'r')).get_first_table()
            except:
               print("No USNO stars found for: {} {}\n".format(ra,dec))
               continue
	    select = t.array['Bmag'] < 9.3
            Rmag = t.array['Bmag'][select]
            min_mag = max(Rmag.min(), 11)
            scale = 0.5 * 10 ** ((min_mag - Rmag) / 2.5)
            ax.scatter(t.array['RAJ2000'][select], t.array['DEJ2000'][select], s=scale, marker='o', facecolor='y', alpha=0.3, edgecolor='',
                       zorder=-10)

    for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
        planet.compute(plot_date)
        ax.scatter(math.degrees(planet.ra), math.degrees(planet.dec),
                   marker='o',
                   s=40,
                   facecolor='r',
                   edgecolor='g', )
        if LABEL_PLANETS :
            ax.text(math.degrees(planet.ra), math.degrees(planet.dec),
                planet.name,
                horizontalalignment='center',
                fontdict={'size': 6,
                          'color': 'darkred'})

if PLOT_MEGACAM_ARCHIVE_FIELDS:
    print("PLOTTING FOOTPRINTS NEARBY ARCHIVAL MEGAPRIME IMAGES.")

    for qra in range(int(ra_cen - width/2.0),int(ra_cen + width/2.), 60):
        for qdec in range(int(dec_cen - height/2.0),int(dec_cen + height/2.), 30):
            filename = USER + "megacam{:+6.2f}{:+6.2f}.xml".format(qra, qdec)
            if not os.access(filename, os.R_OK):
                data = megacam.TAPQuery(qra, qdec, 60.0, 30.0).read()
                fobj = open(filename, 'w')
                fobj.write(data)
                fobj.close()
            fobj = open(filename, 'r')
            t = votable.parse(fobj).get_first_table()

            ra = t.array['RAJ2000']
            dec = t.array['DEJ2000']

            rects = [Rectangle(xy=(ra[idx] - dimen / 2.0, dec[idx] - dimen / 2.0),
                               height=camera_dimen,
                               width=camera_dimen,
                               edgecolor='m',
                               alpha=0.1,
                               lw=0.1, zorder=-100,
                               fill=False) for idx in range(ra.size)]
            for r in rects:
                ax.add_artist(r)

if PLOT_MPCORB:
    print("PLOTTING LOCATIONS OF KNOWN KBOs (using {})".format(MPCORB_FILE))
    kbos = mpcread.getKBOs(MPCORB_FILE)
    for kbo in kbos:
        kbo.compute(plot_date)
        if not ((ra_cen + width / 2.0 > kbo.ra > ra_cen - width / 2.0)
                and (dec_cen + height / 2.0 > kbo.dec > dec_cen - height / 2.0)):
            continue
        ax.scatter(math.degrees(kbo.ra),
                   math.degrees(kbo.dec),
                   marker='h',
                   s=1,
                   facecolor='g',
                   edgecolor='g', alpha=0.3)


if PLOT_REAL_KBOS:
    reader = mpc.MPCReader()
    print("PLOTTING LOCATIONS OF OSSOS KBOs (using directory {})".format(REAL_KBO_AST_DIR))
    for ast in os.listdir(REAL_KBO_AST_DIR):
        obs = reader.read(REAL_KBO_AST_DIR+ast)
        try:
           kbo = orbfit.Orbfit(obs)
        except:
           continue
        kbo.predict(ephem.julian_date(ephem.date(plot_date)))
        #if not field_polygon.isInside(float(kbo.coordinate.ra.degrees), float(kbo.coordinate.dec.degrees)):
        #    continue
        if obs[0].mag < 23.6:
            c = 'b'
            if LABEL_FIELDS:
                ax.text(kbo.coordinate.ra.degree,
                    kbo.coordinate.dec.degree,
                    ast.split('.')[0],
                    horizontalalignment='center',
                    verticalalignment='baseline',
                    fontdict={'size': 6,
                              'color': 'darkred'})
        else:
            c = 'g'
        ax.text(kbo.coordinate.ra.degree,
                kbo.coordinate.dec.degree,
                ast.split('.')[0],
                horizontalalignment='center',
                verticalalignment='baseline',
                fontdict={'size': 6,
                'color': 'darkred'})
        ax.add_artist(Ellipse((kbo.coordinate.ra.degree, kbo.coordinate.dec.degree,),
                              width=2.0*kbo.dra / 3600.0,
                              height=2.0*kbo.ddec / 3600.0,
                              angle=360 - (kbo.pa + 90),
                              edgecolor='none',
                              facecolor='k',
                              alpha=0.2))
        ax.scatter(kbo.coordinate.ra.degree,
                   kbo.coordinate.dec.degree,
                   marker='o',
                   s=1,
                   facecolor='none',
                   edgecolor=c,
                   alpha=0.5)


new_tick_locations = np.array(list(range(360,0,-30)))
def tick_function(X):
    import calendar
    month = ( X/30.0 + 8 ) % 12 + 1
    return [ calendar.month_abbr[int(z)] for z in month ]
print(new_tick_locations)
if False:
    ax.set_xlim(360,0)
    ax2 = ax.twiny()
    ax2.set_xticks(new_tick_locations)
    ax2.set_xticklabels(tick_function(new_tick_locations))
    ax2.set_xlabel("Opp. Month")
    ax2.set_xlim(360,0)


print("SAVING FILE")
savefig('layout.pdf')

sys.stderr.write("FINISHED\n")

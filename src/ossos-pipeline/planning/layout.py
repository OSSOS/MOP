from math import sqrt
import operator
import math
import sys
import os
import string
from ossos import mpc
from ossos import orbfit

from matplotlib.patches import Rectangle
from matplotlib.patches import Ellipse
from matplotlib import rcParams

import numpy as np

from matplotlib.pyplot import figure, savefig, xlabel, ylabel
from astropy.io import votable
import ephem
import Polygon
import Polygon.IO

import mpcread
import usnoB1

import megacam
from ossos import storage


def plot_line(axes, fname, ltype):
    """plot the ecliptic plane line on the given axes."""
    x = np.genfromtxt(fname, unpack=True)
    axes.plot(x[0], x[1], ltype)


PLOT_USNO_STARS = True
PLOT_MEGACAM_ARCHIVE_FIELDS = False
PLOT_SYNTHETIC_KBOS = True
PLOT_SYNTHETIC_KBO_TRAILS = False
REAL_KBO_AST_DIR = '/Users/jjk/Dropbox/dbaseclone/ast'
PLOT_REAL_KBOS = True and os.access(REAL_KBO_AST_DIR,os.F_OK)
PLOT_FIELD_LAYOUT = True
PLOT_FIELD_EPOCH = 'Jun14.00'  # Jun14.00 ==> '0' days since the New Moon on Jun14

MPCORB_FILE = os.path.join(os.getenv('HOME', '/Users/jjk'), 'MPCORB.DAT')
PLOT_MPCORB = True and os.access(MPCORB_FILE, os.F_OK)


# # EmulateApJ columnwidth=245.26 pts
fig_width_pt = 246.0 * 3.0
inches_per_pt = 1.0 / 72.27
golden_mean = (sqrt(5.) - 1.0) / 2.0
fig_width = fig_width_pt * inches_per_pt
fig_height = fig_width * golden_mean * 1.5
fig_size = [fig_width, fig_height]

params = {'backend': 'pdf',
          'axes.labelsize': 10,
          'text.fontsize': 10,
          'legend.fontsize': 10,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          'text.usetex': False,
          'font.serif': 'Times',
          'font.family': 'serif',
          'image.aspect': 'auto',
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
blocks = {'13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"}}  # ,
#        '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"}}

#spring14

DISCOVERY_NEW_MOON = 'Oct13'

newMoons = {'Feb13': "2013/02/10 10:00:00",
            'Mar13': "2013/03/11 10:00:00",
            'Apr13': "2013/04/10 10:00:00",
            'May13': "2013/05/09 10:00:00",
            'Jun13': "2013/06/08 10:00:00",
            'Jul13': "2013/07/08 10:00:00",
            'Aug13': "2013/08/06 10:00:00",
            'Sep13': '2013/09/05 10:00:00',
            'Oct13': '2013/10/04 10:00:00',
            'Nov13': '2013/11/03 10:00:00',
            'Dec13': '2013/12/02 10:00:00',
            'Jan14': '2014/01/01 10:00:00',
            'Feb14': '2014/01/31 10:00:00',
            'Mar14': '2014/03/28 10:00:00',
            'Apr14': '2014/04/01 10:00:00',
            'May14': '2014/05/28 10:00:00',
            'Jun14': '2014/06/26 10:00:00',
            'Jul14': '2014/07/26 10:00:00',
            'Aug14': "2014/08/25 10:00:00",
            'Sep14': '2014/09/24 10:00:00',
            #'Oct14': '2014/10/23 10:00:00',
            #'Nov14': '2014/11/22 10:00:00',
            #'Dec14': '2014/12/22 10:00:00',
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

fix = open('Fall13.xml', 'w')
fix.write(header)

# build the pointings that will be used for the discovery fields
# This is a bit tricky as we keep the heliocentric longitude constant while moving in RA
# and get latitude coverage by changing the DEC.
# Compute the coverage of the discovery fields.
field_names = []
for block in blocks.keys():
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
            coverage.append(Polygon.Polygon((
                (xcen - dimen / 2.0, ycen - dimen / 2.0),
                (xcen - dimen / 2.0, ycen + dimen / 2.0),
                (xcen + dimen / 2.0, ycen + dimen / 2.0),
                (xcen + dimen / 2.0, ycen - dimen / 2.0),
                (xcen - dimen / 2.0, ycen - dimen / 2.0))))

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

if year == "astr":
    ## we don't do trailing for the astr ometric grid, or make any plots...
    sys.exit(0)

ras = np.array(ras)
decs = np.array(decs)

## lets make a plot of the field selections.
fig = figure()
ax = fig.add_subplot(111, aspect="equal")
ax.set_xlim(20, 5)
ax.set_ylim(0, 10)
xlabel('RA (deg)')
ylabel('DE (deg)')
ax.grid()

## plot the galactic plane line ..
plot_line(ax, 'eplane.radec', 'b-')
plot_line(ax, 'gplane.radec', 'g-')

## build a list of Synthetic KBOs that will be in the discovery fields.
ra = []
dec = []
kbos = []
lines = storage.open_vos_or_local('vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt').read().split('\n')
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
    date = ephem.date(newMoons[DISCOVERY_NEW_MOON])
    kbo.compute(date)

    ### only keep objects that are brighter than limit
    if kbo.mag > 25.0:
        continue

    ## keep a list of KBOs that are in the discovery pointings
    for field in coverage:
        if field.isInside(math.degrees(float(kbo.ra)), math.degrees(float(kbo.dec))):
            kbos.append(kbo)
        if PLOT_SYNTHETIC_KBOS:
            ax.scatter(math.degrees(float(kbo.ra)), math.degrees(float(kbo.dec)), c='k', marker='.', s=1, alpha=0.8)
        break

## Now we work out how far to move the fields at different lunations
seps = {}
dates = {}

## build a list of dates that we will need field locations for.
for month in newMoons:
    for night in range(-7, 8):
        epoch = "%s.%s" % (month, string.zfill(night, 2))
        dates[epoch] = ephem.date(ephem.date(newMoons[month]) + night)
        seps[epoch] = {'dra': 0, 'ddec': 0}

## Compute the typical motion of KBOs that were in the discovery field.
for kbo in kbos:
    ra = kbo.ra
    dec = kbo.dec
    for epoch in dates:
        date = ephem.date(dates[epoch])
        kbo.compute(date)
        seps[epoch]['dra'] += kbo.ra - ra
        seps[epoch]['ddec'] += kbo.dec - dec

## plot source locations at the start
## middle and end of semester
if PLOT_SYNTHETIC_KBO_TRAILS:
    colours = {'Jun14': 'g', 'Jul14': 'b', 'Aug14': 'r'}
    alpha = {'Jun14': 0.3, 'Jul14': 0.7, 'Aug14': 0.3}
    zorder = {'Jun14': 1, 'Jul14': 5, 'Aug14': 2}
    for month in ['Jun14', 'Jul14', 'Aug14']:
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

sorted_epochs = sorted(dates.iteritems(), key=operator.itemgetter(1))

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
        if epoch == PLOT_FIELD_EPOCH and PLOT_FIELD_LAYOUT:
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - dimen / 2.0, math.degrees(dec) - dimen / 2.0),
                                    height=camera_dimen,
                                    width=camera_dimen,
                                    edgecolor='b',
                                    lw=0.5, fill=True, alpha=0.3))

    f.write("""]]</CSV></DATA>\n</TABLE>\n</ASTRO>\n""")
    f.close()


ra_cen = 15.0
dec_cen = 5.0
width = 245 - 205
height = -8 + 25

if PLOT_USNO_STARS:

    t = votable.parse(usnoB1.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()

    Rmag = t.array['Bmag'][t.array['Bmag'] < 12]
    min_mag = max(Rmag.min(), 11)
    scale = 0.5 * 10 ** ((min_mag - Rmag) / 2.5)
    ax.scatter(t.array['RAJ2000'], t.array['DEJ2000'], s=scale, marker='o', facecolor='y', alpha=0.8, edgecolor='',
               zorder=-10)

    for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
        planet.compute(ephem.date(newMoons['Jul14']))
        ax.scatter(math.degrees(planet.ra), math.degrees(planet.dec),
                   marker='o',
                   s=30,
                   facecolor='none',
                   edgecolor='g', )

if PLOT_MEGACAM_ARCHIVE_FIELDS:
    t = votable.parse(megacam.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()

    ra = t.array['RAJ2000']
    dec = t.array['DEJ2000']

    rects = [Rectangle(xy=(ra[idx] - dimen / 2.0, dec[idx] - dimen / 2.0),
                       height=camera_dimen,
                       width=camera_dimen,
                       edgecolor='k',
                       alpha=0.1,
                       lw=0.1, zorder=-100,
                       fill=False) for idx in xrange(ra.size)]
    for r in rects:
        ax.add_artist(r)

        ra_cen = math.radians(ra_cen)
        dec_cen = math.radians(dec_cen)
        width = math.radians(width)
        height = math.radians(height)

if PLOT_MPCORB:
    kbos = mpcread.getKBOs(MPCORB_FILE)
    for kbo in kbos:
        kbo.compute(dates[PLOT_FIELD_EPOCH])
        if not ((ra_cen + width / 2.0 > kbo.ra > ra_cen - width / 2.0)
                and (dec_cen + height / 2.0 > kbo.dec > dec_cen - height / 2.0)):
            continue
        ax.scatter(math.degrees(kbo.ra),
                   math.degrees(kbo.dec),
                   marker='h',
                   s=10,
                   facecolor='none',
                   edgecolor='g')

if PLOT_REAL_KBOS:
    for ast in os.listdir(REAL_KBO_AST_DIR):
        if "O13B" not in ast:
            continue
        obs = []
        for line in open(REAL_KBO_AST_DIR+ast):
            obs.append(mpc.Observation.from_string(line))
        kbo = orbfit.Orbfit(obs)
        kbo.predict(ephem.julian_date(ephem.date(PLOT_FIELD_EPOCH)))
        if obs[0].mag < 23.6:
            c = 'b'
            ax.text(kbo.coordinate.ra.degrees,
                    kbo.coordinate.dec.degrees,
                    ast)
        else:
            c = 'g'
        print kbo
        print ast, kbo.coordinate, kbo.dra, kbo.ddec, kbo.pa
        ax.add_artist(Ellipse((kbo.coordinate.ra.degrees, kbo.coordinate.dec.degrees,),
                              width=kbo.dra / 3600.0, height=kbo.ddec / 3600.0, angle=kbo.pa + 90.0,
                              edgecolor='none', facecolor='k', alpha=0.3))
        ax.scatter(kbo.coordinate.ra.degrees,
                   kbo.coordinate.dec.degrees,
                   marker='h',
                   s=10,
                   facecolor='none',
                   edgecolor=c,
                   alpha=0.5)

savefig('layout.png')

sys.stderr.write("FINISHED\n")

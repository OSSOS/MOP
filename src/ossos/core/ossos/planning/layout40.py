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

from . import invariable
from . import mpcread
from . import usnoB1
from . import megacam
from ossos import storage
from ossos import orbfit
from ossos import mpc


USER = os.getenv('HOME','/Users/michele/')

# USER = '/Users/jjk/'

def plot_line(axes, fname, ltype):
    """plot the ecliptic plane line on the given axes."""
    x = np.genfromtxt(fname, unpack=True)
    axes.plot(x[0], x[1], ltype)


MPCORB_FILE = os.path.join(USER, 'MPCORB-Distant.dat')
# MPCORB_FILE = os.path.join(USER, 'Dropbox/Develop/code/MPCORB.DAT')
L7MODEL = 'vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt'
# L7MODEL = '/Users/kavelaarsj/Dropbox/Research/KuiperBelt/OSSOS/L7SyntheticModel-v09.txt'
REAL_KBO_AST_DIR = '/Users/jjk/Dropbox/Research/KuiperBelt/OSSOS/dbaseclone/ast'
#REAL_KBO_AST_DIR = os.path.join(USER, 'Dropbox/OSSOS/measure3/ossin/')

PLOT_FIELD_EPOCH = 'Oct16'  # Oct14.00 ==> '0' days since the New Moon on Oct14
#TODO the .00 is appended when this variable is used as a keyword that needs that .00 this is bad.
DISCOVERY_NEW_MOON = 'Nov15'  # this is the date that the RA/DEC in blocks corresponds to.

PLOT_USNO_STARS = True
PLOT_MEGACAM_ARCHIVE_FIELDS = True
PLOT_SYNTHETIC_KBOS = True
PLOT_SYNTHETIC_KBO_TRAILS = False
PLOT_REAL_KBOS = True and os.access(REAL_KBO_AST_DIR, os.F_OK)
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
# blocks = {'13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"}}  # ,
# '13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},  # E+0+0: image 1616681, ccd21 on April 9
#           '13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8
#           '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"},
#           '14AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"}
#           }

## this position is derived from the +0+0 field for 13B observed on November 1st 2013
# blocks = {'14BH': {'RA': "01:28:32.32", "DEC": "+12:51:06.10"}}

#fall 2015
blocks = {'15BD': {'RA': "03:15:00.00", "DEC": "+16:30:00.00"}}

#spring15
# blocks = {'15AP': {'RA': "13:30:00", "DEC": "-07:45:00"}}
# blocks = {'14AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"}}  # the centre when set in May 2014.
# will then define a 15AM to work properly.
# blocks = {'15AM': {'RA': "15:35:00.00", "DEC": "-12:10:00.0"}}  # the centre for discovery in May 2015.
# blocks = {'15AM': {'RA': "15:35:00.00", "DEC": "-12:10:00.0"}}  # the centre for discovery in May 2015.
# blocks = {
# '14BH': {'RA': "01:28:32.32", "DEC": "+12:51:06.10"},
#     '13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},
#     '15BS': {'RA': "00:30:00.00", "DEC": "+04:45:00.00"}}

blocks = {
    # E+0+0: image 1616681, ccd21 on April 9. E block discovery triplets are April 4,9,few 19
    # '13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},
    #    '13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8. O block are
    # May 7,8.
    # '13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},  # 13B blocks are at their opposition locations
    # '14BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"},  # due to bad weather, discovery wasn't until 2014, so 14

    #    '15AP': {'RA': "13:30:00.00", "DEC": "-7:45:00.00"},  # on-plane
    # '15AM': {'RA': "15:35:00.00", "DEC": "-12:10:00.0"}  # positioned for its 2015 discovery opposition.
    #  '15BS': {'RA': "00:30:00.00", "DEC": "+05:00:00.00"},  # rejected: dec "-02:45:00.00"
       '15BD': {'RA': "03:15:00.00", "DEC": "+16:30:00.00"}
}

newMoons = {
#    'Feb13': "2013/02/10 10:00:00",
#    'Mar13': "2013/03/11 10:00:00",
#    'Apr13': "2013/04/10 10:00:00",
#    'May13': "2013/05/09 10:00:00",
#    'Jun13': "2013/06/08 10:00:00",
#    'Jul13': "2013/07/08 10:00:00",
#    'Aug13': "2013/08/06 10:00:00",
#    'Sep13': '2013/09/05 10:00:00',
    # 'Oct13': '2013/10/04 10:00:00',
#    'Nov13': '2013/11/03 10:00:00',
#    'Dec13': '2013/12/02 10:00:00',
#    'Jan14': '2014/01/01 10:00:00',
#    'Feb14': '2014/01/31 10:00:00',
#    'Mar14': '2014/03/28 10:00:00',
#    'Apr14': '2014/04/01 10:00:00',
# 'May14': '2014/05/28 10:00:00',
#    'Jun14': '2014/06/26 10:00:00',
#    'Jul14': '2014/07/26 10:00:00',
#    'Aug14': "2014/08/25 10:00:00",
#    'Sep14': '2014/09/24 10:00:00',
#    'Oct14': '2014/10/23 10:00:00',
#    'Nov14': '2014/11/22 10:00:00',
#    'Dec14': '2014/12/22 10:00:00',
#    'Jan15': '2015/01/20 10:00:00',
#    'Feb15': '2015/02/18 10:00:00',
#    'Mar15': '2015/03/19 10:00:00',
#    'Apr15': '2015/04/18 10:00:00',
#    'May15': '2015/05/17 10:00:00',
#    'Jun15': '2015/06/16 10:00:00',
#    'Jul15': '2015/07/15 10:00:00',
#    'Aug15': '2015/08/14 10:00:00',
 #   'Sep15': '2015/09/12 10:00:00',
#    'Oct15': '2015/10/12 10:00:00',
    'Nov15': '2015/11/11 10:00:00',
#    'Dec15': '2015/12/11 10:00:00',
#    'Jan16': '2016/01/09 10:00:00',
#    'Feb16': '2016/03/08 10:00:00',
#    'Mar16': '2016/03/09 10:00:00',
#    'Apr16': '2016/04/08 10:00:00',
#    'May16': '2016/05/07 10:00:00',
#    'Jun16': '2016/06/05 10:00:00',
#    'Jul16': '2016/07/05 10:00:00',
#    'Aug16': '2016/08/03 10:00:00',
    'Sep16': '2016/09/01 10:00:00',
    'Oct16': '2016/10/01 10:00:00',
#    'Nov16': '2016/11/01 10:00:00',
#    'Dec16': '2016/12/01 10:00:00',
#    'Jan17': '2017/01/01 10:00:00',
    # 'Feb17': '2017/02/01 10:00:00',
    }



xgrid = {'2014': [-3, -2, -1, 0, 1, 2, 3],
         '2014r': [-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5],
         'astr': [1, 2, 3, 4],
         '40ccd': [2, 1, 0, -1, -2]}
# 2013
#       'astr':[-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5]}

ygrid = {'2014': [-1, 0, 1],
         'astr': [-2.5, -1.5, -0.5, 0.5, 1.5],
         '2014r': [-1.5, -0.5, 0.5, 1.5],
         '40ccd': [-1.5, -0.5, 0.5, 1.5]}
# 2013
#       'astr':  [-2.2,-1.2, -0.2, 0.8, 1.8],

block_centre = ephem.Ecliptic(0, 0)
field_centre = ephem.Ecliptic(0, 0)

## foffset is the offset (in RA and DEC) between fields
field_offset = math.radians(1.00)
# which year, for the x/y offset grids, are we using.
year = '40ccd'

# the 'astr' year indicates astrometric grid,  this should have more overlap so reduce the field_offset for those.
if year == 'astr':
    field_offset *= 0.75

## camera_dimen is the size of the field.
## this is the 40 CD Layout.  The width is set to exlcude one of the 
## Ears.  dimen is really the height
pixscale = 0.184
detsec_ccd09 = ((4161, 2114), (14318, 9707))
detsec_ccd10 = ((6279, 4232), (14317, 9706))
detsec_ccd36 = ((2048, 1), (14318, 9707))
detsec_ccd37 = ((23219, 21172), (14309, 9698))
detsec_ccd00 = ((4160, 2113), (19351, 14740))
detsec_ccd17 = ((21097, 19050), (14309, 9698))
detsec_ccd35 = ((16934, 18981), (11, 4622))

print(detsec_ccd09[0][0] - detsec_ccd10[0][1], detsec_ccd09[0][1] - detsec_ccd36[0][0])

camera_width =  (max(detsec_ccd37[0]) - min(detsec_ccd09[0]) + 1 )* pixscale / 3600.0
camera_height = (max(detsec_ccd00[1]) - min(detsec_ccd35[1]) + 1 )* pixscale / 3600.0
camera_width_40 =  (max(detsec_ccd37[0]) - min(detsec_ccd36[0]) + 1 )* pixscale / 3600.0
camera_width_36 = (max(detsec_ccd17[0]) - min(detsec_ccd09[0]) + 1 ) * pixscale / 3600.0 

print(camera_width, camera_width_40, camera_width_36, camera_height)

years = {"2014": {"ra_off": ephem.hours("00:00:00"),
                  "dec_off": ephem.hours("00:00:00"),
                  "fill": False,
                  "facecolor": 'k',
                  "alpha": 0.5,
                  "color": 'b'},
         "40ccd": {"ra_off": ephem.hours("00:00:00"),
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
    ra_start = ephem.hours(blocks[block]["RA"]) + years[year]["ra_off"]
    dec_start = ephem.degrees(blocks[block]["DEC"]) + years[year]["dec_off"]
    height = math.radians(camera_height)
    idy = 0
    for dx in xgrid[year]:
        idx = int(dx)
        for dy in ygrid[year]:
            idy = int(math.floor(dy))
            decc = ephem.degrees(dec_start + dx*height/2.0)
            this_decc = decc + dy * height
            width = 1.007*math.radians(camera_width / math.cos(this_decc))
            ## set to a -ve instead of +ve for the 'fall'
            rac = ephem.hours(ra_start + dx * width)
            dec = math.degrees(this_decc)
            ra = math.degrees(rac)
            print("{} {} {}".format(rac, dec, math.degrees(width)))
            field_name = "%s%+d%+d" % (block, idx, idy)
            field_names.append(field_name)
            decs.append(np.radians(dec))
            ras.append(np.radians(ra))
            xcen = ra
            ycen = dec
            fix.write('%20s|%10s|%10s|2000.0|    1|\n' % (field_name,
                                                          ephem.hours(math.radians(ra)),
                                                          ephem.degrees(math.radians(dec))))
            dimenw = math.degrees(width)
            dimenh = math.degrees(height)
            this_point_polygon = Polygon.Polygon((
                (xcen - dimenw / 2.0, ycen - dimenh / 2.0),
                (xcen - dimenw / 2.0, ycen + dimenh / 2.0),
                (xcen + dimenw / 2.0, ycen + dimenh / 2.0),
                (xcen + dimenw / 2.0, ycen - dimenh / 2.0),
                (xcen - dimenw / 2.0, ycen - dimenh / 2.0)))
            field_polygon = field_polygon is None and this_point_polygon or this_point_polygon | field_polygon


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

### These values are the half width/height of the field.
width = 24
height = 12

## lets make a plot of the field selections.
fig = figure()
ax = fig.add_subplot(111)
# ax.set_aspect('equal')

# xylims = [250, 195, -17, -4]  # all of A semester
# xylims = [27, 3, -6, 17]  # H, L and low S blocks
# xylims = [26, 3, 0, 16]  # H, L and high S blocks
xylims = [60, 40, 12, 21]  # D block
ax.set_xlim(xylims[0], xylims[1])  # ra_cen + width, ra_cen - width)
ax.set_ylim(xylims[2], xylims[3])  # dec_cen - height, dec_cen + height)
#ax.set_ylim(-30,30)
ax.set_xlabel('RA (deg)')
ax.set_ylabel('DE (deg)')

ax.grid()


## plot the galactic plane line ..
plot_line(ax, 'eplane.radec', 'b-')
plot_line(ax, 'gplane.radec', 'g-')
ax.text(20, 8.5, 'ecliptic', fontdict={'color': 'b'})

# plot the invariant plane
lon = np.arange(-2 * math.pi, 2 * math.pi, 0.5 / 57)
lat = 0 * lon
(lat, lon) = invariable.trevonc(lat, lon)

ec = [ephem.Ecliptic(x, y) for (x, y) in np.array((lon, lat)).transpose()]
eq = [ephem.Equatorial(coord) for coord in ec]
ax.plot([math.degrees(coord.ra) for coord in eq],
        [math.degrees(coord.dec) for coord in eq],
        '-k',
        lw=1,
        alpha=0.7)
ax.text(25, 7.5, 'invariant', fontdict={'color': 'k'})

(cc_lat, cc_lon) = invariable.trevonc(lat, lon, chiangchoi_plane=True)
ec = [ephem.Ecliptic(x, y) for (x, y) in np.array((cc_lon, cc_lat)).transpose()]
eq = [ephem.Equatorial(coord) for coord in ec]
ax.plot([math.degrees(coord.ra) for coord in eq],
        [math.degrees(coord.dec) for coord in eq],
        '.r',
        lw=1,
        alpha=0.7)
ax.text(25, 6, 'Chiang_Choi', fontdict={'color': 'r'})


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
        if kbo.mag < 25.0:
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
    if month != PLOT_FIELD_EPOCH:
        continue
    for night in range(-9, 9):
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

camera_width=0.983*camera_width
camera_height=0.983*camera_height

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
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - camera_width_40/2.0, math.degrees(dec) - camera_height / 4.0 ),
                                    width= camera_width_40,
                                    height= camera_height/2.0,
                                    color='b',
                                    lw=0.5, fill=False, alpha=0.3)) 
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - camera_width_36/2.0, math.degrees(dec) - camera_height / 2.0),
                                    height=camera_height/4.0,
                                    width=camera_width_36,
                                    color='g',
                                    lw=0.5, fill=False, alpha=0.3))
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - camera_width_36/2.0, math.degrees(dec) + camera_height / 4.0),
                                    height=camera_height/4.0,
                                    width=camera_width_36,
                                    color='r',
                                    lw=0.5, fill=False, alpha=0.3))
            if LABEL_FIELDS:
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
    for ra in range(int(ra_cen - width), int(ra_cen + width), 10):
        for dec in range(int(dec_cen - height), int(dec_cen + height), 10):
	    file_name = USER + "/new_usno/usno{:5.2f}{:5.2f}.xml".format(ra, dec).replace(" ", "")
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

    for qra in range(int(xylims[1]), int(xylims[0]), 5):
        print(qra)
        for qdec in range(int(xylims[2]), int(xylims[3]), 10):
            print(qra, qdec)
            filename = USER + "/new_usno/megacam{:6.2f}{:6.2f}.xml".format(qra, qdec)
            if not os.access(filename, os.R_OK):
                # TAP query will max out silently if return table too large. Make small units.
                data = megacam.TAPQuery(qra, qdec, 5.0, 10.0).read()
                print(data)
                # FIXME: add a catch for 503's when TAP fails when CADC drops connection
                fobj = open(filename, 'w')
                fobj.write(data)
                fobj.close()
            fobj = open(filename, 'r')
            t = votable.parse(fobj).get_first_table()

            ra = t.array['RAJ2000']
            dec = t.array['DEJ2000']

            rects = [Rectangle(xy=(ra[idx] - camera_width_36 / 2.0, dec[idx] - camera_width_36 / 2.0),
                               height=camera_height,
                               width=camera_width_36,
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
        if field_polygon.isInside(math.degrees(float(kbo.ra)), math.degrees(float(kbo.dec))):
            ax.scatter(math.degrees(kbo.ra),
                       math.degrees(kbo.dec),
                       marker='x',
                       s=4,
                       facecolor='r',
                       edgecolor='r', alpha=0.3)

if PLOT_REAL_KBOS:
    reader = mpc.MPCReader()
    print("PLOTTING LOCATIONS OF OSSOS KBOs (using directory {})".format(REAL_KBO_AST_DIR))
    for ast in os.listdir(REAL_KBO_AST_DIR):
        if not ast.startswith('u'):
            obs = reader.read(REAL_KBO_AST_DIR + ast)
            try:
                kbo = orbfit.Orbfit(obs)
            except:
                continue
            kbo.predict(ephem.julian_date(ephem.date(plot_date)))
            # if not field_polygon.isInside(float(kbo.coordinate.ra.degrees), float(kbo.coordinate.dec.degrees)):
            #    continue
            if obs[0].mag < 23.6:
                c = 'b'
                if LABEL_FIELDS:
                    ax.text(kbo.coordinate.ra.degree,
                            kbo.coordinate.dec.degree,
                            ast.split('.')[0],
                            horizontalalignment='center',
                            verticalalignment='baseline',
                            fontdict={'size': 4,
                                      'color': 'darkred'})
            else:
                c = 'g'
                ax.text(kbo.coordinate.ra.degree,
                        kbo.coordinate.dec.degree,
                        ast.split('.')[0],
                        horizontalalignment='center',
                        verticalalignment='baseline',
                        fontdict={'size': 4,
                                  'color': 'darkred'})
            # from astropy import units
            ax.add_artist(Ellipse((kbo.coordinate.ra.degree, kbo.coordinate.dec.degree,),
                                  width=2.0 * kbo.dra / 3600.0,
                                  height=2.0 * kbo.ddec / 3600.0,
                                  angle=360 - (kbo.pa + 90),  # kbo.pa.to(units.degree).value
                                  edgecolor='none',
                                  facecolor='k',
                                  alpha=0.1))
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
savefig('layout40-at' + PLOT_FIELD_EPOCH + '-discov_on-' + DISCOVERY_NEW_MOON + '.pdf')

sys.stderr.write("FINISHED\n")

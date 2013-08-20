

import ephem
from matplotlib.patches import Rectangle
from matplotlib.projections import register_projection
from math import sqrt
import numpy as np
import mpcread
from matplotlib.pyplot import figure, show, savefig, xlabel, ylabel
from matplotlib import cm
from matplotlib import rcParams
from astropy.io import votable
import operator
import usnoB1
import megacam
import ephem
import math
import sys
import os
from numpy.random import random
import Polygon, Polygon.IO
import string

## EmulateApJ columnwidth=245.26 pts                                                                                                   
fig_width_pt = 246.0
inches_per_pt = 1.0/72.27
golden_mean = (sqrt(5.)-1.0)/2.0
fig_width = fig_width_pt*inches_per_pt
fig_height= fig_width*golden_mean*1.5
fig_size =[fig_width, fig_height]

params = {'backend': 'pdf',
          'axes.labelsize': 10,
          'text.fontsize': 10,
          'legend.fontsize': 10,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          'text.usetex': False,
          'font.serif': 'Times',
#          'font.family': 'serif',
          'image.aspect': 'auto',
          'figure.subplot.left': 0.2,
          'figure.subplot.bottom': 0.15,
          'figure.figsize': fig_size}

#rcParams.update(params)

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

header="""<?xml version = "1.0"?>
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



#spring13
blocks={'E':{"RA": "14:32:30.29","DEC":"-13:54:01.4"},
        'O':{"RA": "16:17:04.41","DEC":"-13:14:45.8"}}
#fall13
blocks={'13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"}, 
        '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"}}

newMoons={'Feb13': "2013/02/10 10:00:00",
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
          'Feb14': '2014/01/31 10:00:00'}

xgrid={'2013':[-3, -2, -1, 0, 1, 2, 3],
       '2014':[-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5],
       'astr':[1,2,3,4]}
# 2013
#       'astr':[-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5]}

ygrid={'2013':  [-1, 0, 1],
       'astr':  [-2.5,-1.5,-0.5,0.5,1.5],
       '2014':  [-1.5,-0.5, 0.5, 1.5]}
# 2013
#       'astr':  [-2.2,-1.2, -0.2, 0.8, 1.8],

block_centre = ephem.Ecliptic(0,0)
field_centre = ephem.Ecliptic(0,0)

## foffset is the offset (in RA and DEC) between fields
field_offset = math.radians(1.00)
## camera_dimen is the size of the field.
camera_dimen=0.98

years={"2013": {"ra_off": ephem.hours("00:00:00"),
               "dec_off":ephem.hours("00:00:00"),
	       "fill": False,
	       "facecolor": 'k',
               "alpha": 0.5,
               "color": 'b'},
       "2014": {"ra_off": ephem.hours("00:05:00"),
                "dec_off":ephem.degrees("00:18:00"),
                "alpha": 0.5,
	        "fill": False,
	        "facecolor": 'k',
                "color": 'g'},
       'astr': {"ra_off": ephem.hours("00:00:00"),
                "dec_off": ephem.degrees("-00:12:00"),
                "alpha": 0.05,
                "fill": True,
	        'facecolor': 'k',
                "color": 'none'}
       }



# build the pointings that will be used for the discovery fields

x = []
y = []
names = []
coverage = []
year = '2013'
#year = 'astr'
if year == 'astr':
  field_offset = field_offset*0.75
fix = open('Fall13.xml', 'w')
fix.write(header)

fig=figure()
ax=fig.add_subplot(111,aspect="equal")

for block in blocks.keys():
  sign = -1
  if 'f' in block:
    sign=1
  rac = ephem.hours(blocks[block]["RA"])+years[year]["ra_off"]
  decc = ephem.degrees(blocks[block]["DEC"])+sign*years[year]["dec_off"]
  width = field_offset/math.cos(decc)
  block_centre.from_radec(rac,decc)
  block_centre.set(block_centre.lon + xgrid[year][0]*width,block_centre.lat)
  field_centre.set(block_centre.lon, block_centre.lat)
  for dx in xgrid[year]:  
    (rac, decc) = field_centre.to_radec()
    for dy in ygrid[year]:
      ddec = dy*field_offset
      dec = math.degrees(decc + ddec)
      ra = math.degrees(rac)
      names.append("%s%+d%+d" % ( block, dx, dy))
      y.append(dec)
      x.append(ra)
      xcen = ra
      ycen = dec
      fix.write('%20s|%10s|%10s|2000.0|    1|\n' % ( names[-1],ephem.hours(math.radians(ra)),ephem.degrees(math.radians(dec))))

      dimen = camera_dimen
      coverage.append(Polygon.Polygon((
            (xcen-dimen/2.0,ycen-dimen/2.0),
            (xcen-dimen/2.0,ycen+dimen/2.0),
            (xcen+dimen/2.0,ycen+dimen/2.0),
            (xcen+dimen/2.0,ycen-dimen/2.0),
            (xcen-dimen/2.0,ycen-dimen/2.0))))
      print ra, dec
      ax.add_artist(Rectangle(xy=(ra-dimen/2.0,dec-dimen/2.0), 
                              height=camera_dimen, 
                              width=camera_dimen, 
                              edgecolor='b', 
                              lw=0.5, fill=True, alpha=0.3))

    rac += field_offset / math.cos(decc)
    for i in range(3):
      field_centre.from_radec(rac,decc)
      field_centre.set(field_centre.lon, block_centre.lat)
      (ttt, decc) = field_centre.to_radec()

fix.write("""]]</CSV></DATA>
</TABLE>
</ASTRO>
""")

if year == "astr":
  sys.exit(0)
      
ras = np.radians(x)
decs = np.radians(y)


ax.grid()

## plot the ecliptic plane line ..
lines = open('eplane.radec').readlines()
x=[]
y=[]
for line in lines:
   v= line.split()
   x.append(float(v[0]))
   y.append(float(v[1]))
ax.plot(x,y,'b-')

## plot the galactic plane line ..
lines = open('gplane.radec').readlines()
x=[]
y=[]
for line in lines:
   v= line.split()
   x.append(float(v[0]))
   y.append(float(v[1]))
ax.plot(x,y,'g-')


## build a list of KBOs that will be in the discovery fields.
ra=[]
dec=[]
p=[]
q=[]

kbos = []
for line in open('L7SyntheticModel-v09.txt'):
   if line[0]=='#':
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
   kbo._epoch_M = ephem.date(2453157.50000 - ephem.julian_date(0))
   kbo._epoch = kbo._epoch_M
   kbo.name = values[8]
   date = ephem.date(newMoons['Oct13'])
   kbo.compute(date)


   ### make a seperate plot of the p/q value of the cold objects
   if values[9] in ['k','s']:
     p.append(math.sin(kbo._inc)*math.sin(kbo._Om))
     q.append(math.sin(kbo._inc)*math.cos(kbo._Om))

   ### only keep objects that are brighter than limit
   if kbo.mag > 25.0:
     continue
   ra.append(math.degrees(float(kbo.ra)))
   dec.append(math.degrees(float(kbo.dec)))

   ## keep a list of KBOs that are in the discovery pointings
   for field in coverage:
     if field.isInside(ra[-1],dec[-1]):
       kbos.append(kbo)
       break

ax.scatter(ra,dec,c='k',marker='.',s=1,alpha=0.8 )
x=[]
y=[]

## Now we work out how far to move the fields at different lunations
seps = {}
dates = {}

for month in newMoons:
  for night in range(-7,8):
    epoch = "%s.%s" % ( month, string.zfill(night,2))
    dates[epoch] = ephem.date(ephem.date(newMoons[month]) + night)
    seps[epoch]={'dra': 0, 'ddec': 0}


for kbo in kbos:
  ra=kbo.ra
  dec=kbo.dec
  for epoch in dates:
    date = ephem.date(dates[epoch])
    kbo.compute(date)
    seps[epoch]['dra'] += kbo.ra - ra
    seps[epoch]['ddec'] += kbo.dec - dec

## plot source locations at the start
## middle and end of semester
colours  = {'Aug13': 'g', 'Oct13': 'b', 'Dec13': 'r'}
alpha = {'Aug13': 0.3, 'Oct13': 0.7, 'Dec13': 0.3}
zorder = {'Aug13': 1, 'Oct13': 5, 'Dec13': 2}
for month in ['Aug13', 'Oct13', 'Dec13']:
  ra = []
  dec = []
  date = ephem.date(newMoons[month])
  for kbo in kbos:
    kbo.compute(date)
    ra.append(math.degrees(kbo.ra))
    dec.append(math.degrees(kbo.dec))
  #ax.plot(ra,dec,colours[month]+'.', alpha=alpha[month], zorder=zorder[month])

## Now plot the boxes for each month, shifting
## from the discovery set by the average motion of
## L7 kbos in the discovery field
for month in seps:
  seps[month]['dra'] /= float(len(kbos))
  seps[month]['ddec'] /= float(len(kbos))

sorted_epochs = sorted(dates.iteritems(), key=operator.itemgetter(1))

for idx in range(len(ras)):
  name = names[idx]
  f = file('%s.xml' % ( name), 'w')
  #f2 = file('%s.no.xml' % ( name), 'w')
  #f2.write(etHeader)
  f.write(etHeader)
  for epoch in sorted_epochs:
    epoch = epoch[0]
    date = dates[epoch]
    ra = ras[idx] + seps[epoch]['dra']
    dec = decs[idx] + seps[epoch]['ddec']
    #f.write('%20s|%10s|%10s|2000.0|    1|\n' % ( name[-1],ephem.hours(ra),ephem.degrees(dec)))
    tdate = date.tuple()
    zf = string.zfill
    
    sdate = "%4s-%2s-%2s %2s:%2s:%2s" % ( tdate[0], zf(tdate[1],2), zf(tdate[2],2), zf(tdate[3],2),zf(tdate[4],2),zf(int(tdate[5]),2))
    f.write('%19s|%11s|%11s|\n' % ( sdate,ephem.hours(ra),ephem.degrees(dec)))
    #f2.write('%19s|%11s|%11s|\n' % ( sdate,ephem.hours(ra),ephem.degrees(dec+math.radians(90.0/3600.0))))
    

  f.write("""]]</CSV></DATA>
</TABLE>
</ASTRO>
""")
#  f2.write("""]]</CSV></DATA>
#</TABLE>
#</ASTRO>
#""")

ra_cen = (37+7)/2.0
dec_cen = (20+0)/2.0
width = 20-0
height = 37-7

ax.set_xlim(37,7)
ax.set_ylim(0,20)

t = votable.parse(usnoB1.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()

Rmag = t.array['Bmag'][t.array['Bmag'] < 15]
min = max(Rmag.min(),11)
max = Rmag.max()
scale = 0.5*10**((min - Rmag)/2.5)
print scale.min(), scale.max()
ax.scatter(t.array['RAJ2000'], t.array['DEJ2000'], s=scale, marker='o', facecolor='y', alpha=0.8, edgecolor='', zorder=-10)

for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
  planet.compute(ephem.date(newMoons['Oct13']))
  ax.scatter(math.degrees(planet.ra), math.degrees(planet.dec),
             marker='o',
             s=30,
             facecolor='none',
             edgecolor='g',)

xlabel('RA (deg)')
ylabel('DE (deg)')

t = votable.parse(megacam.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()


ra = t.array['RAJ2000']
dec = t.array['DEJ2000']


rects = [Rectangle(xy=(ra[idx]-dimen/2.0,dec[idx]-dimen/2.0),
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

MPCORB = os.path.join(os.getenv('HOME','/Users/jjk'), 'MPCORB.DAT')
if os.access(MPCORB,os.F_OK):
   kbos = mpcread.getKBOs(MPCORB)
   for kbo in kbos:
      kbo.compute(ephem.date(newMoons['Oct13']))
      if not ( kbo.ra < ra_cen + width/2.0 and kbo.ra > ra_cen - width/2.0 and kbo.dec < dec_cen + height/2.0 and kbo.dec > dec_cen - height/2.0 ) :
         continue
      ax.scatter(math.degrees(kbo.ra),
             math.degrees(kbo.dec),
             marker='h',
             s=10,
             facecolor='none',
             edgecolor='g')

   

savefig('layout.pdf')

sys.stderr.write("FINISHED\n")


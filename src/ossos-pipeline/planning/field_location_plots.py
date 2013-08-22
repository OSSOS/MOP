from math import sqrt
import operator
import math
import os
import sys
import string
import numpy as np
import ephem
from astropy.io import votable
import Polygon

from matplotlib.patches import Rectangle
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt

# planning scripts
import mpcread
import usnoB1
import megacam

# ORIGINAL: FIXME not matching the location of the data!
# Ablocks={'13AE': {"RA": "14:32:30.29","DEC":"-13:54:01.4"},
#         '13AO': {"RA": "16:17:04.41","DEC":"-13:14:45.8"}}

# Ablocks are HACKED for the purpose of plotting: DO NOT USE for pointings generation
Ablocks = {'13AE': {"RA": "14:15:00","DEC":"-13:30:00"},
        '13AO': {"RA": "16:00:00.00","DEC":"-14:0:00.00"}}

# trustworthy
Bblocks = {'13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},
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

ygrid={'2013':  [-1, 0, 1],
       'astr':  [-2.5,-1.5,-0.5,0.5,1.5],
       '2014':  [-1.5,-0.5, 0.5, 1.5]}

block_centre = ephem.Ecliptic(0,0)
field_centre = ephem.Ecliptic(0,0)

# the offset (in RA and DEC) between fields
field_offset = math.radians(1.00)
# the size of the field
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

# FIXME: redo this to have brewermpl colours?
def colsym():
    # needs a light grey to get the original populations shown as background.
    mcolours = ['0.75', 'k', 'yellow', 'black', 'blue'] # closer to 1 is lighter. 0.85 is good.
    msymbols = ['o', 'o', 'o', 'x', 'd']

    return mcolours, msymbols


def basic_skysurvey_plot_setup():
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

    fig = plt.figure()
    ax = fig.add_subplot(111, aspect="equal")
    handles = []  # this handles the creation of the legend properly at the end of plotting
    labels = []
    handles, labels = plot_galactic_plane(handles, labels)
    handles, labels = plot_ecliptic_plane(handles, labels)

    fontP = FontProperties()
    fontP.set_size('small')   # make the fonts smaller
    plt.xlabel('RA (deg)')
    plt.ylabel('Declination (deg)')
    # shrink current axis's height by 10% on the bottom to fit legend easily
#    box = ax.get_position()
#    ax.set_position([box.x0, box.y0 + box.height * 0.1, box.width, box.height * 0.9])
    plt.grid(True)

    return handles, labels, ax, fontP


def plot_galactic_plane(handles, labels):
    gp = [ephem.Galactic(str(lon), str(0)) for lon in range(0, 360)]
    galPlane = [ephem.Equatorial(coord) for coord in gp]
    # rewrap the ra to start at zero: that otherwise causes a problem in the plotting!
    temp = [(math.degrees(coord.ra), math.degrees(coord.dec)) for coord in galPlane]
    temp.sort(key=lambda x:x[0])
    ra_galPlane = [t[0] for t in temp]
    dec_galPlane = [tt[1] for tt in temp]
    handles.append(plt.plot(ra_galPlane, dec_galPlane, 'b'))
    labels.append('galactic plane')
    #plt.plot([r+360 for r in ra_galPlane], dec_galPlane, 'b')  # echo

    return handles, labels


def plot_ecliptic_plane(handles, labels):
    ep = [ephem.Ecliptic(str(lon), str(0)) for lon in range(0, 360)]  # this line is fine: need Ecliptic(long, lat) format in creation
    ecPlane =  [ephem.Equatorial(coord) for coord in ep]  # so this must be where the issue is.
    ra_ecPlane = [math.degrees(coord.ra) for coord in ecPlane]
    dec_ecPlane = [math.degrees(coord.dec) for coord in ecPlane]
    handles.append(plt.plot(ra_ecPlane, dec_ecPlane, '#E47833'))
    labels.append('ecliptic')
    #plt.plot([rr+360 for rr in ra_ecPlane], dec_ecPlane, '#E47833')  # echo

    return handles, labels

def plot_planets(handles, labels, plot, date):
#     # only add the planets that would actually fall in this plot
#     plot_polygon = Polygon.Polygon(((plot[0],plot[2]),
#                                    (plot[0],plot[3]),
#                                    (plot[1],plot[3]),
#                                    (plot[0],plot[3]),
#                                    (plot[0],plot[2])))
# #    print plot_polygon

    for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
        planet.compute(ephem.date(date))
        pos = (math.degrees(planet.ra), math.degrees(planet.dec))
#        if plot_polygon.isInside(math.degrees(planet.ra), math.degrees(planet.dec)):
        handles.append(plt.scatter(pos[0], pos[1],
                 marker='o',
                 s=30,
                 facecolor='none',
                 edgecolor='g',))
        handles.append(plt.annotate(planet.name, (pos[0]+0.7, pos[1]+0.7)))  # offset to make it readable
#        labels.append(planet.name)

    return handles, labels


def build_ossos_footprint(ax, blocks, field_offset):
    # build the pointings that will be used for the discovery fields
    x = []
    y = []
    names = []
    coverage = []
    year = '2013'
    if year == 'astr':
      field_offset = field_offset*0.75

    for block in blocks.keys():
      sign = -1
      if 'f' in block:  # unsure what this was ever meant to do
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

          dimen = camera_dimen
          coverage.append(Polygon.Polygon((
                (xcen-dimen/2.0,ycen-dimen/2.0),
                (xcen-dimen/2.0,ycen+dimen/2.0),
                (xcen+dimen/2.0,ycen+dimen/2.0),
                (xcen+dimen/2.0,ycen-dimen/2.0),
                (xcen-dimen/2.0,ycen-dimen/2.0))))

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


    if year == "astr":
      sys.exit(0)

    ras = np.radians(x)
    decs = np.radians(y)

    return ras, decs, coverage, names, ax


def synthetic_model_kbos(coverage, input_date=newMoons['Oct13']):
    ## build a list of KBOs that will be in the discovery fields.
    ra=[]
    dec=[]
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
       date = ephem.date(input_date)
       kbo.compute(date)

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

    return ra, dec, kbos

def plot_synthetic_kbos(ax, coverage):
    ra, dec, kbos = synthetic_model_kbos(coverage)
    ax.scatter(ra,dec,c='k',marker='.',s=1,alpha=0.8 )

    return ax

def keplerian_sheared_field_locations(ax, kbos, opp_date, names):
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
#    colours  = {'Aug13': 'g', 'Oct13': 'b', 'Dec13': 'r'}
#    alpha = {'Aug13': 0.3, 'Oct13': 0.7, 'Dec13': 0.3}
#    zorder = {'Aug13': 1, 'Oct13': 5, 'Dec13': 2}
    for month in ['Aug13', 'Oct13', 'Dec13']:
      ra = []
      dec = []
      date = ephem.date(newMoons[month])
      for kbo in kbos:
        kbo.compute(date)
        ra.append(math.degrees(kbo.ra))
        dec.append(math.degrees(kbo.dec))
#      ax.plot(ra,dec,colours[month]+'.', alpha=alpha[month], zorder=zorder[month])

    ## Now plot the boxes for each month, shifting
    ## from the discovery set by the average motion of
    ## L7 kbos in the discovery field
    for month in seps:
      seps[month]['dra'] /= float(len(kbos))
      seps[month]['ddec'] /= float(len(kbos))

    sorted_epochs = sorted(dates.iteritems(), key=operator.itemgetter(1))

    for idx in range(len(ras)):
      name = names[idx]
#      f = file('%s.xml' % ( name), 'w')
      #f2 = file('%s.no.xml' % ( name), 'w')
      #f2.write(etHeader)
      # f.write(etHeader)
      for epoch in sorted_epochs:
        epoch = epoch[0]
        date = dates[epoch]
        ra = ras[idx] + seps[epoch]['dra']
        dec = decs[idx] + seps[epoch]['ddec']
        tdate = date.tuple()
        zf = string.zfill

        sdate = "%4s-%2s-%2s %2s:%2s:%2s" % ( tdate[0], zf(tdate[1],2), zf(tdate[2],2), zf(tdate[3],2),zf(tdate[4],2),zf(int(tdate[5]),2))
 #       f.write('%19s|%11s|%11s|\n' % ( sdate,ephem.hours(ra),ephem.degrees(dec)))


def plot_USNO_B1_stars(ax):
    t = votable.parse(usnoB1.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()
    Rmag = t.array['Bmag'][t.array['Bmag'] < 15]
    min = max(Rmag.min(),11)
    max = Rmag.max()
    scale = 0.5*10**((min - Rmag)/2.5)
    print scale.min(), scale.max()
    ax.scatter(t.array['RAJ2000'], t.array['DEJ2000'], s=scale, marker='o', facecolor='y', alpha=0.8, edgecolor='', zorder=-10)

    return ax


def plot_existing_CFHT_Megacam_observations_in_area(ax):
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

    return ax


def plot_known_tnos(ax, ra_cen, dec_cen, width, height, date=newMoons['Oct13']):
    MPCORB = 'MPCORB.DAT'  # TNOs file only: http://www.minorplanetcenter.net/iau/MPCORB/Distant.txt
    rate_cut = 'a > 15'
    if os.access(MPCORB,os.F_OK):
       kbos = mpcread.getKBOs(MPCORB, cond=rate_cut)
       for kbo in kbos:
          kbo.compute(ephem.date(date))
          ax.scatter(math.degrees(kbo.ra),
                 math.degrees(kbo.dec),
                 marker='+',
                 facecolor='none',
                 edgecolor='g')

    return ax


if __name__ == '__main__':

    block_dates = ['Apr13','Oct13']  # block oppositions
    plot_extents = [[245,208,-20,-10], [30,5,-2,18]]

    for ii, block in enumerate([Ablocks, Bblocks]):
        handles, labels, ax, fontP = basic_skysurvey_plot_setup()
        handles, labels = plot_planets(handles, labels, plot_extents[ii], newMoons[block_dates[ii]])

        # this works fine for October but is lies for April due to the initial pointings being odd...
        ras, decs, coverage, names, ax = build_ossos_footprint(ax, block, field_offset)
    #    ra, dec, kbos = synthetic_model_kbos(coverage, input_date=newMoons[block_dates[ii]])
    #    ax = keplerian_sheared_field_locations(ax, kbos, block_dates[ii], names)

        ax = plot_known_tnos(ax, date=newMoons[block_dates[ii]])

        plt.axis(plot_extents[ii])  # this will be dependent on the survey fields being plotted
        # plt.legend(handles, labels, prop=fontP, loc='upper center', bbox_to_anchor=(0.5, -0.2), ncol=3)

        plt.show()
#        plt.draw()
        outfile = block_dates[ii]
        plt.savefig(outfile+'.pdf', transparent=True)#, bbox_inches='tight')
    sys.stderr.write("Finished.\n")

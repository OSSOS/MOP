from math import sqrt
import math
import os
import sys
import argparse

import numpy as np
import ephem
from astropy.io import votable
import Polygon
import datetime
from matplotlib.patches import Rectangle
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt


# planning scripts
import mpcread
import usnoB1
import megacam

from track_done import parse, get_names
from ossos import storage
HAVE_HORIZONS=True
try:
    from ossos import horizons
except:
    HAVE_HORIZONS=False
    



# ORIGINAL: DON't USE, does not match the location of the data as pointed & taken!
# Ablocks={'13AE': {"RA": "14:32:30.29","DEC":"-13:54:01.4"},
#         '13AO': {"RA": "16:17:04.41","DEC":"-13:14:45.8"}}

# 13A pointings taken from pointing of actual +0+0 data: DO NOT USE for pointings generation (probably)
# E block discovery triplets are April 4,9,few 19; O block are May 7,8.
BLOCKS = {'13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},  # E+0+0: image 1616681, ccd21 on April 9
          '13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8
          # 13B are trustworthy: this was what we agreed
          '13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},  # 13B blocks are at their opposition locations
          '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"},  # due to bad weather, discovery wasn't until Dec/Jan
          # 14A fields:  These are the 'pre-covery' fields  for 15A 
          '14AM': {'RA': "15:36:00.00", "DEC": "-12:00:00.0"}
          '15AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"}
}

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
            'Jun14': '2014/06/26 10:00:00'
            }

xgrid = {'2013': [-3, -2, -1, 0, 1, 2, 3],
         '2014r': [-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5],
         '2014': [-3, -2, -1, 0, 1, 2, 3],
         'astr': [1, 2, 3, 4]}

ygrid = {'2013': [-1, 0, 1],
         '2014': [-1, 0, 1],
         'astr': [-2.5, -1.5, -0.5, 0.5, 1.5],
         '2014r': [-1.5, -0.5, 0.5, 1.5]}

block_centre = ephem.Ecliptic(0, 0)
field_centre = ephem.Ecliptic(0, 0)

# the offset (in RA and DEC) between fields
field_offset = math.radians(1.00)
# the size of the field
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
                  "color": 'none'}
}

saturn_moons = ['Iapetus', 'Phoebe', 'Ymir', 'Paaliaq', 'Tarvos', 'Ijiraq', 'Suttungr', 'Kiviuq', 'Mundilfari',
                'Albiorix', 'Skathi', 'Erriapus', 'Siarnaq', 'Thrymr', 'Narvi', 'Bestla', 'Hyrrokkin', 'Kari']

# FIXME: redo this to have brewermpl colours?
def colsym():
    # needs a light grey to get the original populations shown as background.
    mcolours = ['0.75', 'k', 'yellow', 'black', 'blue']  # closer to 1 is lighter. 0.85 is good.
    msymbols = ['o', 'o', 'o', 'x', 'd']

    return mcolours, msymbols


def basic_skysurvey_plot_setup():
    ## EmulateApJ columnwidth=245.26 pts
    fig_width_pt = 246.0
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
    fontP.set_size('small')  # make the fonts smaller
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
    temp.sort(key=lambda x: x[0])
    ra_galPlane = [t[0] for t in temp]
    dec_galPlane = [tt[1] for tt in temp]
    handles.append(plt.plot(ra_galPlane, dec_galPlane, 'b'))
    labels.append('galactic plane')
    #plt.plot([r+360 for r in ra_galPlane], dec_galPlane, 'b')  # echo

    return handles, labels


def plot_ecliptic_plane(handles, labels):
    ep = [ephem.Ecliptic(str(lon), str(0)) for lon in
          range(0, 360)]  # this line is fine: need Ecliptic(long, lat) format in creation
    ecPlane = [ephem.Equatorial(coord) for coord in ep]  # so this must be where the issue is.
    ra_ecPlane = [math.degrees(coord.ra) for coord in ecPlane]
    dec_ecPlane = [math.degrees(coord.dec) for coord in ecPlane]
    handles.append(plt.plot(ra_ecPlane, dec_ecPlane, '#E47833'))
    labels.append('ecliptic')
    #plt.plot([rr+360 for rr in ra_ecPlane], dec_ecPlane, '#E47833')  # echo

    return handles, labels


def plot_planets(ax, plot, date, hill_sphere=False):
    #     # only add the planets that would actually fall in this plot
    #     plot_polygon = Polygon.Polygon(((plot[0],plot[2]),
    #                                    (plot[0],plot[3]),
    #                                    (plot[1],plot[3]),
    #                                    (plot[0],plot[3]),
    #                                    (plot[0],plot[2])))
    # #    print plot_polygon
    mass = {"Sun": 1.989 * 10 ** 30, "Mars": 639 * 10 ** 21, "Jupiter": 1.898 * 10 ** 27, "Saturn": 568.3 * 10 ** 24,
            "Uranus": 86.81 * 10 ** 24, "Neptune": 102.4 * 10 ** 24}  # kg
    for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
        planet.compute(ephem.date(date))
        pos = (math.degrees(planet.ra), math.degrees(planet.dec))
        #        if plot_polygon.isInside(math.degrees(planet.ra), math.degrees(planet.dec)):
        ax.scatter(pos[0], pos[1],
                   marker='o',
                   s=30,
                   facecolor='#E47833',
                   edgecolor='#E47833')
        ax.annotate(planet.name, (pos[0] + .4, pos[1] + 0.15))  #(pos[0]+.9, pos[1]+0.5))  # offset to make it readable

        if hill_sphere:
            print planet.name, planet.sun_distance, mass[planet.name],
            hs_radius = (planet.sun_distance * ephem.meters_per_au) * (
            (mass[planet.name] / 3 * mass['Sun']) ** (1 / 3.))
            angular_size = planet.earth_distance * hs_radius  # FIXME
            print 'Hill sphere', hs_radius, hs_radius / ephem.meters_per_au, angular_size
            ax.add_patch(plt.Circle(pos, radius=angular_size, fill=False))

    return ax


def build_ossos_footprint(ax, blocks, field_offset, plot=True):
    # build the pointings that will be used for the discovery fields
    x = []
    y = []
    names = []
    coverage = []
    year = '2014'
    if year == 'astr':
        field_offset = field_offset * 0.75

    for block in blocks.keys():
        sign = -1
        if 'f' in block:  # unsure what this was ever meant to do
            sign = 1
        rac = ephem.hours(blocks[block]["RA"]) + years[year]["ra_off"]
        decc = ephem.degrees(blocks[block]["DEC"]) + sign * years[year]["dec_off"]
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
                names.append("%s%+d%+d" % ( block, dx, dy))
                y.append(dec)
                x.append(ra)
                xcen = ra
                ycen = dec

                dimen = camera_dimen
                coverage.append(Polygon.Polygon((
                    (xcen - dimen / 2.0, ycen - dimen / 2.0),
                    (xcen - dimen / 2.0, ycen + dimen / 2.0),
                    (xcen + dimen / 2.0, ycen + dimen / 2.0),
                    (xcen + dimen / 2.0, ycen - dimen / 2.0),
                    (xcen - dimen / 2.0, ycen - dimen / 2.0))))

                if plot:
                    ax.add_artist(Rectangle(xy=(ra - dimen / 2.0, dec - dimen / 2.0),
                                            height=dimen,
                                            width=dimen,
                                            edgecolor='b',
                                            lw=0.5, fill=True, alpha=0.2))

            rac += field_offset / math.cos(decc)
            for i in range(3):
                field_centre.from_radec(rac, decc)
                field_centre.set(field_centre.lon, block_centre.lat)
                (ttt, decc) = field_centre.to_radec()

    ras = np.radians(x)
    decs = np.radians(y)

    return ras, decs, coverage, names, ax


def synthetic_model_kbos(coverage, input_date=newMoons['Oct13']):
    ## build a list of KBOs that will be in the discovery fields.
    ra = []
    dec = []
    kbos = []
    lines = storage.open_vos_or_local('vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt').read().split('\n')
    for line in lines:
        if (len(line) > 0 and line[0] == '#') or (
            len(line) == 0):  # skip initial column descriptors and the final blank line
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
            if field.isInside(ra[-1], dec[-1]):
                kbos.append(kbo)
                break

    return ra, dec, kbos


def plot_synthetic_kbos(ax, coverage):
    ra, dec, kbos = synthetic_model_kbos(coverage)
    ax.scatter(ra, dec, c='k', marker='.', s=1, alpha=0.8)

    return ax


def keplerian_sheared_field_locations(ax, kbos, date, ras, decs, names, elongation=False, plot=False):
    """
    Shift fields from the discovery set to the requested date by the average motion of L7 kbos in the discovery field.
    :param ras:
    :param decs:
    :param plot:
    :param ax:
    :param kbos: precomputed at the discovery date for that block. e.g. Oct new moon for 13B
    :param date:
    :param names:
    :param elongation:
    """
    seps = {'dra': 0., 'ddec': 0.}
    for kbo in kbos:
        ra = kbo.ra
        dec = kbo.dec
        kbo.compute(date)
        seps['dra'] += kbo.ra - ra
        seps['ddec'] += kbo.dec - dec

    seps['dra'] /= float(len(kbos))
    seps['ddec'] /= float(len(kbos))

    print date, seps

    for idx in range(len(ras)):
        name = names[idx]
        ra = ras[idx] + seps['dra']
        dec = decs[idx] + seps['ddec']
        if plot:
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - camera_dimen / 2.0, math.degrees(dec) - camera_dimen / 2.0),
                                    height=camera_dimen,
                                    width=camera_dimen,
                                    edgecolor='b',
                                    lw=0.5, fill=True, alpha=0.2))
        if elongation:
            # For each field centre, plot the elongation onto the field at that date.
            elong = field_elongation(ephem.degrees(ra), ephem.degrees(dec), date)
            ax.annotate(name, (math.degrees(ra) + camera_dimen / 2., math.degrees(dec)), size=3)
            ax.annotate("%0.1f" % elong, (math.degrees(ra) + camera_dimen / 4., math.degrees(dec) - camera_dimen / 4.),
                        size=5)

    return ax


def field_elongation(ra, dec, date):
    """
    For a given field, calculate the solar elongation at the given date.
    :param ra: field's right ascension. unit="h"   format="RAh:RAm:RAs"
    :param dec: field's declination. degrees
    :param date: date at which to calculate elongation
    :return: elongation from the Sun in degrees
    """
    sun = ephem.Sun()
    sun.compute(date)
    sep = ephem.separation((ra, dec), sun)
    retval = 180. - math.degrees(sep)

    return retval


def plot_USNO_B1_stars(ax):
    t = votable.parse(usnoB1.TAPQuery(ra_cen, dec_cen, width, height)).get_first_table()
    Rmag = t.array['Bmag'][t.array['Bmag'] < 15]
    min = max(Rmag.min(), 11)
    max = Rmag.max()
    scale = 0.5 * 10 ** ((min - Rmag) / 2.5)
    print scale.min(), scale.max()
    ax.scatter(t.array['RAJ2000'], t.array['DEJ2000'], s=scale, marker='o', facecolor='y', alpha=0.8, edgecolor='',
               zorder=-10)

    return ax


def plot_existing_CFHT_Megacam_observations_in_area(ax):
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

    return ax


def plot_known_tnos_batch(handles, labels, date):
    MPCORB = 'MPCORB.DAT'  # TNOs file only: http://www.minorplanetcenter.net/iau/MPCORB/Distant.txt
    rate_cut = 'a > 15'
    if os.access(MPCORB, os.F_OK):
        kbos = mpcread.getKBOs(MPCORB, cond=rate_cut)
        kbo_ra = []
        kbo_dec = []
        for kbo in kbos:
            kbo.compute(ephem.date(date))
            kbo_ra.append(math.degrees(kbo.ra))
            kbo_dec.append(math.degrees(kbo.dec))
        handles.append(plt.scatter(kbo_ra,
                                   kbo_dec,
                                   marker='+',
                                   facecolor='none',
                                   edgecolor='g'))
        labels.append('known TNOs')

    return handles, labels


def plot_known_tnos_singly(ax, date):
    MPCORB = 'MPCORB.DAT'  # TNOs file only: http://www.minorplanetcenter.net/iau/MPCORB/Distant.txt
    rate_cut = 'a > 15'
    if os.access(MPCORB, os.F_OK):
        kbos = mpcread.getKBOs(MPCORB, cond=rate_cut)
        for kbo in kbos:
            kbo.compute(ephem.date(date))
            pos = (math.degrees(kbo.ra), math.degrees(kbo.dec))
            ax.scatter(pos[0],
                       pos[1],
                       marker='x',
                       facecolor='r')
            ax.annotate(kbo.name, (pos[0] + .4, pos[1] + 0.1), size='xx-small', color='r')

    return ax


def plot_ossos_discoveries(ax, blockID='O13AE', date="2013/04/09 08:50:00"):
    path = 'vos:OSSOS/measure3/2013A-E/track/discoveries/'
    discoveries = storage.listdir(path)
    names = get_names(path, blockID)
    for kbo in names:
        arclen, orbit = parse(kbo, discoveries, path=path)
        if date != "2013/04/09 08:50:00":  # date is the new moon for a given month
            orbit.predict(date.replace('/', '-'))
            ra = orbit.coordinate.ra.degrees
            dec = orbit.coordinate.dec.degrees
        else:  # specific date on which discovery was made: use discovery locations
            ra = orbit.observations[0].coordinate.ra.degrees
            dec = orbit.observations[0].coordinate.dec.degrees
        ax.scatter(ra, dec, marker='+', facecolor='k')
        ax.annotate(kbo.replace(blockID, ''),
                    (orbit.coordinate.ra.degrees + .05, orbit.coordinate.dec.degrees - 0.2),
                    size='xx-small',
                    color='k')

    return ax


def plot_single_known_tno(ax, name, date, close=[]):
    # date formatted as %Y-%m-%d %H:%M
    if not HAVE_HORIZONS:
        return ax
    dateplus1hr = (datetime.datetime.strptime(date, '%Y-%m-%d %H:%M') + datetime.timedelta(1 / 24.)).strftime(
        '%Y-%m-%d %H:%M')
    obj_elems, single_ephem = horizons.batch(name, date, dateplus1hr, None, su='d')  # pull back one position only
    ra = math.degrees(ephem.degrees(ephem.hours(single_ephem[0]['RA'])))
    dec = math.degrees(ephem.degrees(single_ephem[0]['DEC']))
    ax.scatter(ra,
               dec,
               marker='.',
               facecolor='r',
               edgecolor='r',
               alpha=0.4)
    if name in close:  # keep the names from falling on top of each other
        if name == 'Hyrrokkin':
            ax.annotate(name, (ra - .02, dec), size='5', color='r')
        if name == 'Ijiraq':
            ax.annotate(name, (ra - .05, dec - 0.015), size='5', color='r')
        if name == 'Bestla':
            ax.annotate(name, (ra - .05, dec - 0.01), size='5', color='r')
    else:
        ax.annotate(name, (ra - .05, dec - 0.01), size='5', color='r')

    return ax


def plot_saturn_moons(ax):
    for s_moon in saturn_moons:  # get Saturn's irregular moons
        # 'Kiviuq' to left. Hyr mag 24.3, Bestla mag 24.5, Kiviuq 22.6, Iji 23.3
        close = ['Ijiraq', 'Bestla', 'Hyrrokkin']
        ax = plot_single_known_tno(ax, s_moon, "2013-04-09 08:50", close=close)

    return ax


def plot_discovery_uncertainty_ellipses(ax, date):
    # plot objects at time of triplet with an X, location each 3 days with a single grey pixel, actual observations
    # with a solid dot. Will give suitable corkscrew effect.
    # plot wireframe of footprint at discovery triplet
    # plot hollow circle as location at start of dark run, ellipse (no fill I think) for orbit uncertainty.
    return ax


def saving_closeout(date, extents, file_id):
    plt.title(date)
    plt.axis(extents)  # depends on which survey field is being plotted
    plt.draw()
    outfile = 'plots/' + + file_id + date.replace('/', '-') + '.pdf'
    # Always use axes.set_rasterized(True) if you are saving as an EPS file.
    print 'Saving file.', outfile
    plt.savefig(outfile, transparent=True)

    return


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-opposition", "-op", action="store_true",
                        help="Plots blocks at their opposition dates (the date .")
    parser.add_argument("-discovery", "-d", action="store_true",
                        help="Plots blocks at their discovery dates.")
    parser.add_argument("-date", action="store_true",
                        help="Plot blocks at a specific user-provided date, format yyyy/mm/dd HH:MM:SS.")
    parser.add_argument("-dist", action="store_true",
                        help="Display text of block IDs and arc distance of blocks from opposition on the given date")
    parser.add_argument("-blocks", action="store_true",
                        help="specify blocks to be plotted, e.g. 13AE. Without specifying, will do all N(blocks) that"
                             "exist, making N(blocks) separate plots.")
    args = parser.parse_args()

    elongation = False
    block_dates = []
    file_id = ""
    blocks = BLOCKS  # Coordinates at opposition of currently-determined OSSOS blocks (2013: 4; 2015: 8 total).
    if args.blocks:
        blocks = BLOCKS[args.blocks]
    print blocks

    for extent, block in enumerate(blocks):  # FIXME: need separation between key, item here in blocks
        if args.opposition:
            # 13AE is a month different in opposition from 13AO, but 13B are both in October
            block_dates = [newMoons['Apr13'], newMoons['May13'], newMoons['Oct13'], newMoons['Oct13'], newMoons['May14']]
            # FIXME: correct 13AO extent, tweak 13B extents into individual blocks
            plot_extents = [[219, 209, -16, -9], [219, 209, -16, -9], [30, 5, -2, 18], [30, 5, -2, 18], [219, 209, -16, -9]]
            # 13A all: [[245, 208, -18, -8]] 13B all: [30, 5, -2, 18]
            file_id = 'opposition-'

        if args.discovery:
            # E block discovery triplets are April 4,9,few 19; O block are May 7,8.
            # FIXME: 13H/L discovery dates, 13AO discovery time
            block_dates = ["2013/04/09 08:50:00", "2013/05/08 08:50:00", "2013/12/12 08:50:00", "2014/01/03 08:50:00", "2014/05/28 08:50:00"]
            plot_extents = [[219, 209, -16, -9], [219, 209, -16, -9], [30, 5, -2, 18], [30, 5, -2, 18], [219, 209, -16, -9]]
            file_id = 'discovery-'

        if args.date:
            # if it's not a list (multiple dates), wrap the single date into being a list
            if not args.date.isinstance('list'):
                block_dates = [args.date]
                # FIXME: calculate the appropriate extents given that date and the given blocks
            plot_extents = [[219, 209, -16, -9], [219, 209, -16, -9], [30, 5, -2, 18], [30, 5, -2, 18], [219, 209, -16, -9]]
            file_id = args.date

        if args.dist:
            elongation = True
            file_id = 'elongation-'

        for date in block_dates:
            handles, labels, ax, fontP = basic_skysurvey_plot_setup()
            ras, decs, coverage, names, ax = build_ossos_footprint(ax, blocks, field_offset, plot=False)
            #ax = keplerian_sheared_field_locations(ax, kbos, date, ras, decs, names, elongation=elongation, plot=True)
            ax = plot_planets(ax, plot_extents[extent], date)
            ax = plot_ossos_discoveries(ax, date=date)  # will predict for dates other than discovery
            ax = plot_discovery_uncertainty_ellipses(ax, date)

            if args.synthetic:
                # defaults to Oct new moon: FIXME: set date appropriately
                ra, dec, kbos = synthetic_model_kbos(coverage)

            if "A" in block:  # special case: needs Saturn's moons plotted as well
                ax = plot_known_tnos_singly(ax, date)  # this because we need the ax and batch doesn't return that
                ax = plot_saturn_moons(ax)
            else:  # plot as normal
                handles, labels = plot_known_tnos_batch(handles, labels, date)

            saving_closeout(block, date, plot_extents[extent], file_id)

    sys.stderr.write("Finished.\n")



import math
import sys
import argparse
import os

import numpy as np
import ephem
from astropy.io import votable
from astropy.time import Time, TimeDelta
import Polygon
from matplotlib.patches import Rectangle
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt

from planning import megacam
import parsers
import parameters
import plot_fanciness
import plot_objects
from planning import invariable




# from ossos import cameras  # bit over the top to show all the ccds?

plot_extents = {"13AE": [209.8, 218.2, -15.5, -9.5],
                "13AO": [235.4, 243.8, -15.5, -9.5],
                "13A": [209, 244, -22, -9],  # for both 13A blocks together
                "15AM": [229, 235, -15, -9],
                "15AP": [198, 207, -12, -4],
                "13B": [30, 5, -2, 18],  # for both 13B blocks together
                "13BL": [9, 18, 1, 7],
                "14BH": [18, 27, 10, 16],
                "15BD": [44, 54, 13, 20],
                "15BS": [7, 16, -4, 4]
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

# FIXME: redo this to have brewermpl colours?
def colsym():
    # needs a light grey to get the original populations shown as background.
    mcolours = ['0.75', 'k', 'yellow', 'black', 'blue']  # closer to 1 is lighter. 0.85 is good.
    msymbols = ['o', 'o', 'o', 'x', 'd']

    return mcolours, msymbols


def basic_skysurvey_plot_setup(from_plane=ephem.degrees('0')):
    # # EmulateApJ columnwidth=245.26 pts
    # fig_width_pt = 246.0
    # inches_per_pt = 1.0 / 72.27
    # golden_mean = (sqrt(5.) - 1.0) / 2.0
    # fig_width = fig_width_pt * inches_per_pt
    # fig_height = fig_width * golden_mean * 1.5
    # fig_size = [fig_width, fig_height]

    fig = plt.figure()
    ax = fig.add_subplot(111)  # , aspect="equal")

    handles = []  # this handles the creation of the legend properly at the end of plotting
    labels = []
    handles, labels = plot_galactic_plane(handles, labels)
    # handles, labels = plot_ecliptic_plane(handles, labels)
    handles, labels = plot_invariable_plane(handles, labels, from_plane)

    fontP = FontProperties()
    fontP.set_size('small')  # make the fonts smaller
    plt.xlabel('RA (deg)')
    plt.ylabel('Declination (deg)')
    plt.grid(True, which='both')
    # plot_fanciness.remove_border(ax)

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
    # plt.plot([r+360 for r in ra_galPlane], dec_galPlane, 'b')  # echo

    return handles, labels


def plot_ecliptic_plane(handles, labels):
    ep = [ephem.Ecliptic(str(lon), str(0)) for lon in
          range(0, 360)]  # this line is fine: need Ecliptic(long, lat) format in creation
    ecPlane = [ephem.Equatorial(coord) for coord in ep]  # so this must be where the issue is.
    ra_ecPlane = [math.degrees(coord.ra) for coord in ecPlane]
    dec_ecPlane = [math.degrees(coord.dec) for coord in ecPlane]
    handles.append(plt.plot(ra_ecPlane, dec_ecPlane, '#E47833'))
    labels.append('ecliptic')
    # plt.plot([rr+360 for rr in ra_ecPlane], dec_ecPlane, '#E47833')  # echo

    return handles, labels


def plot_invariable_plane(handles, labels, from_plane=ephem.degrees('0')):
    # Plot the invariable plane: values from DE405, table 5, Souami and Souchay 2012
    # http://www.aanda.org/articles/aa/pdf/2012/07/aa19011-12.pdf
    # Ecliptic J2000
    lon = np.arange(-2 * math.pi, 2 * math.pi, 0.5 / 57)
    lat = 0 * lon
    (lat, lon) = invariable.trevonc(lat, lon)

    ec = [ephem.Ecliptic(x, y) for (x, y) in np.array((lon, lat)).transpose()]
    eq = [ephem.Equatorial(coord) for coord in ec]
    handles.append(plt.plot([math.degrees(coord.ra) for coord in eq],
                            [math.degrees(coord.dec) for coord in eq],
                            ls='--',
                            color=plot_fanciness.ALMOST_BLACK,
                            lw=1,
                            alpha=0.7))
    labels.append('invariable')

    return handles, labels


def build_ossos_footprint(ax, block_name, block, field_offset, plot=True, plot_col='b'):
    # build the pointings that will be used for the discovery fields
    x = []
    y = []
    names = []
    coverage = []
    year = '2014'
    if year == 'astr':
        field_offset = field_offset * 0.75

    sign = -1
    # if 'f' in block:  # unsure what this was ever meant to do
    # sign = 1
    rac = ephem.hours(block["RA"]) + years[year]["ra_off"]
    decc = ephem.degrees(block["DEC"]) + sign * years[year]["dec_off"]
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
            names.append("%s%+d%+d" % ( block_name, dx, dy))
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
                                        edgecolor='k',
                                        facecolor=plot_col,
                                        lw=0.5, fill=True, alpha=0.15, zorder=0))

        rac += field_offset / math.cos(decc)
        for i in range(3):
            field_centre.from_radec(rac, decc)
            field_centre.set(field_centre.lon, block_centre.lat)
            (ttt, decc) = field_centre.to_radec()

    ras = np.radians(x)
    decs = np.radians(y)

    return ras, decs, coverage, names, ax


def synthetic_model_kbos(coverage, input_date=parameters.DISCOVERY_NEW_MOON):
    # # build a list of KBOs that will be in the discovery fields.

    raise NotImplementedError('Not yet fully completed.')

    # kbos = parsers.synthetic_model_kbos(input_date)
    # ## keep a list of KBOs that are in the discovery pointings
    #
    #     # FIXME
    #     for field in coverage:
    #         for kbo in kbos:
    #             if kbo.isInside(ra[-1], dec[-1]):
    #                 kbos.append(kbo)
    #                 break
    #
    # return ra, dec, kbos


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

    print date, seps, len(kbos)

    for idx in range(len(ras)):
        name = names[idx]
        ra = ras[idx] + seps['dra']
        dec = decs[idx] + seps['ddec']
        if plot:
            ax.add_artist(Rectangle(xy=(math.degrees(ra) - camera_dimen / 2.0, math.degrees(dec) - camera_dimen / 2.0),
                                    height=camera_dimen,
                                    width=camera_dimen,
                                    edgecolor='b',
                                    facecolor='b',
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


def plot_existing_CFHT_Megacam_observations_in_area(ax, qra, qdec, discovery=None, obs_ids=None):
    filename = '/Users/michele/' + "megacam{:+6.2f}{:+6.2f}.xml".format(qra, qdec)
    if not os.access(filename, os.R_OK):
        data = megacam.TAPQuery(qra, qdec, 60.0, 30.0).read()
        fobj = open(filename, 'w')
        fobj.write(data)
        fobj.close()
    fobj = open(filename, 'r')
    t = votable.parse(fobj).get_first_table()

    ra = t.array['RAJ2000']
    dec = t.array['DEJ2000']
    mjdate = t.array['MJDATE']

    camera_dimen = 0.98

    # More sophisticated date ID on plot. Let's identify first-year, discovery, second-year followup
    for idx in xrange(ra.size):
        alpha = 0.1
        # print abs(Time(mjdate[idx], format='mjd') - discovery) < TimeDelta(1, format='jd')
        if Time(mjdate[idx], format='mjd') < Time('2014-01-01'):
            ec = 'b'
        elif (discovery is not None and abs(Time(mjdate[idx], format='mjd') - discovery) < TimeDelta(1, format='jd')):
            print Time(mjdate[idx], format='mjd').iso
            ec = 'r'
            alpha = 1
        else:
            ec = '#E47833'
        r = Rectangle(xy=(ra[idx] - camera_dimen / 2.0, dec[idx] - camera_dimen / 2.0),
                      height=camera_dimen,
                      width=camera_dimen,
                      edgecolor=ec,
                      alpha=alpha,
                      lw=0.1,
                      zorder=-100,
                      fill=False)
        ax.add_artist(r)

    return ax


def plot_discovery_uncertainty_ellipses(ax, date):
    # plot objects at time of triplet with an X, location each 3 days with a single grey pixel, actual observations
    # with a solid dot. Will give suitable corkscrew effect.
    # plot wireframe of footprint at discovery triplet
    # plot hollow circle as location at start of dark run, ellipse (no fill I think) for orbit uncertainty.
    raise NotImplementedError
    return ax


def saving_closeout(blockname, date, extents, file_id):
    # plt.title(date)
    plt.axis(extents)  # depends on which survey field is being plotted
    plt.draw()
    outfile = blockname + file_id + date.split(' ')[0].replace('/', '-') + '.pdf'
    # Always use axes.set_rasterized(True) if you are saving as an EPS file.
    print 'Saving file.', outfile
    plt.savefig(outfile, transparent=True)
    plt.close()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-opposition",
                        help="Plots blocks at their opposition dates (the date .")
    parser.add_argument("-discovery",
                        action="store_false",
                        help="Plots blocks at their discovery dates.")
    parser.add_argument("-date",
                        help="Plot blocks at a specific user-provided date, format yyyy/mm/dd HH:MM:SS.")
    parser.add_argument("-elongation",
                        help="Display text of block IDs and arc distance of blocks from opposition on the given date")
    parser.add_argument("-synthetic",
                        action="store_true",
                        help="Use L7 model to slew blocks, or plot L7 model objects that fall in a given block.")
    parser.add_argument("-existing",
                        action="store_true",
                        help="Plot all OSSOS imaging of the block.")
    parser.add_argument("blocks",
                        nargs='*',
                        help="specify blocks to be plotted, e.g. 13AE. Without specifying, will do all eight blocks,"
                             "making eight separate plots.")
    args = parser.parse_args()
    print(args)

    file_id = ""
    if args.blocks:  # Coordinates at discovery-opposition of currently-determined OSSOS blocks (2013: 4; 2015: 8
    # total).
        blocks = {}
        for b in args.blocks:
            assert b in parameters.BLOCKS
            blocks[b] = parameters.BLOCKS[b]
    else:  # do them all!
        blocks = parameters.BLOCKS

    discoveries = parsers.ossos_release_parser()  # ossos_release_parser()

    # extent = plot_extents['13A'] # hardwired for doing both at once

    for blockname, block in blocks.items():
        extent = plot_extents[blockname]

        if args.opposition:
            # 13AE is a month different in opposition from 13AO, but 13B are both in October
            date = parameters.OPPOSITION_DATES[blockname]
            file_id = 'opposition-'

        if args.discovery:
            # E block discovery triplets are April 4,9,few 19; O block are May 7,8.
            # BL is 09-29 AND 10-31. Discoveries in 10-31 are placed at predicted locations, to accommodate split.
            date = parameters.DISCOVERY_DATES[blockname]
            file_id = 'discovery-'

        if args.date:
            date = args.date
            args.synthetic = True
            file_id = '-at-date-'

        if args.elongation:
            file_id = 'elongation-'

        if args.existing:
            file_id += '-bg-imaging-'

        assert date
        print blockname, date

        if blockname == '13AO':
            handles, labels, ax, fontP = basic_skysurvey_plot_setup(from_plane=ephem.degrees('6.0'))
            ras, decs, coverage, names, ax = build_ossos_footprint(ax, blockname, block, field_offset, plot=True,
                                                                   plot_col='#E47833')
        else:
            handles, labels, ax, fontP = basic_skysurvey_plot_setup()
            ras, decs, coverage, names, ax = build_ossos_footprint(ax, blockname, block, field_offset, plot=True)
        ax, kbos = plot_objects.plot_known_tnos_singly(ax, extent, date)
        if args.synthetic:  # defaults to discovery new moon: FIXME: set date appropriately
            ra, dec, kbos = synthetic_model_kbos(coverage)
            ax = keplerian_sheared_field_locations(ax, kbos, date, ras, decs, names,
                                                   elongation=args.elongation, plot=True)
            ax = plot_objects.plot_synthetic_kbos(ax, coverage)

        block_discoveries = discoveries[
            np.array([name.startswith('o3' + blockname[-1].lower()) for name in discoveries['object']])]
        ax = plot_objects.plot_ossos_discoveries(ax, block_discoveries, blockname,
                                                 prediction_date=date)  # will predict for dates other than discovery

        # ax = plot_objects.plot_planets(ax, extent, date)

        # ax = plot_discovery_uncertainty_ellipses(ax, date)
        # if blockname == '13AE':  # special case: Saturn was adjacent at discovery
        #     ax = plot_objects.plot_saturn_moons(ax)

        if args.existing:
            plot_existing_CFHT_Megacam_observations_in_area(ax)

        saving_closeout(blockname, date, plot_extents[blockname], file_id)
    # saving_closeout('13A', date, extent, file_id)

    sys.stderr.write("Finished.\n")



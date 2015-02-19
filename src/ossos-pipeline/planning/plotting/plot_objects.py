__author__ = 'Michele Bannister   git:@mtbannister'

import math
import os

import ephem
import matplotlib.pyplot as plt
import datetime

import parameters
import mpcread
import parsers
import horizons



# FIXME: make this an args setting rather than hardwired
PLOT_MPCORB = True and os.access(parameters.MPCORB_FILE, os.F_OK)

saturn_moons = ['Phoebe', 'Ymir', 'Paaliaq', 'Tarvos', 'Ijiraq', 'Suttungr', 'Kiviuq', 'Mundilfari',
                'Albiorix', 'Skathi', 'Erriapus', 'Siarnaq', 'Thrymr', 'Narvi', 'Bestla', 'Hyrrokkin', 'Kari']
# pulled out 'Iapetus', on Saturn


def plot_planets(ax, plot, date, hill_sphere=False):
    mass = {"Sun": 1.989 * 10 ** 30, "Mars": 639 * 10 ** 21, "Jupiter": 1.898 * 10 ** 27, "Saturn": 568.3 * 10 ** 24,
            "Uranus": 86.81 * 10 ** 24, "Neptune": 102.4 * 10 ** 24}  # kg
    for planet in [ephem.Mars(), ephem.Jupiter(), ephem.Saturn(), ephem.Uranus(), ephem.Neptune()]:
        planet.compute(ephem.date(date))
        pos = (math.degrees(planet.ra), math.degrees(planet.dec))
        # if plot_polygon.isInside(math.degrees(planet.ra), math.degrees(planet.dec)):
        ax.scatter(pos[0], pos[1],
                   marker='o',
                   s=30,
                   facecolor='k',  ##E47833',
                   edgecolor='k')  ##E47833')
        ax.annotate(planet.name, (pos[0] - .4, pos[1] + 0.1))  #(pos[0]+.9, pos[1]+0.5))  # offset to make it readable

        if hill_sphere:
            print planet.name, planet.sun_distance, mass[planet.name],
            hs_radius = (planet.sun_distance * ephem.meters_per_au) * (
                (mass[planet.name] / 3 * mass['Sun']) ** (1 / 3.))
            angular_size = planet.earth_distance * hs_radius  # FIXME
            print 'Hill sphere', hs_radius, hs_radius / ephem.meters_per_au, angular_size
            ax.add_patch(plt.Circle(pos, radius=angular_size, fill=False))

    return ax


def plot_saturn_moons(ax):
    for s_moon in saturn_moons:  # get Saturn's irregular moons
        # 'Kiviuq' to left. Hyr mag 24.3, Bestla mag 24.5, Kiviuq 22.6, Iji 23.3
        close = ['Ijiraq', 'Bestla', 'Hyrrokkin', 'Kiviuq']
        ax = plot_single_known_tno(ax, s_moon, "2013-04-09 08:50", close=close)

    return ax


def plot_ossos_discoveries(ax, discoveries, prediction_date=False):  # , blockID='O13AE', date="2013/04/09 08:50:00"):
    for kbo in discoveries:
        if prediction_date:
            kbo.orbit.predict(date.replace('/', '-'))
            ra = kbo.orbit.coordinate.ra.degrees
            dec = kbo.orbit.coordinate.dec.degrees
        else:  # specific date on which discovery was made: use discovery locations
            ra = math.degrees(ephem.degrees(ephem.hours(str(kbo.ra_discov))))
            dec = math.degrees(ephem.degrees(str(kbo.dec_discov)))
        if (kbo.classification == 'res' and kbo.n == 3 and kbo.m == 2):
            print kbo.name
            fc = '#E47833'
            alpha = 1
        else:
            fc = 'b'
            alpha = 0.6
        ax.scatter(ra, dec, marker='o', facecolor=fc, alpha=alpha, edgecolor='k', linewidth=0.4, s=25)
        ax.annotate(kbo.name[3:],
                    (ra - .07, dec - 0.14),  # confirm this is being added properly
                    size=7,
                    color='k')

    return ax


def plot_known_tnos_singly(ax, extent, date):
    rate_cut = 'a > 15'
    print "PLOTTING LOCATIONS OF KNOWN KBOs (using {})".format(parameters.MPCORB_FILE)
    kbos = mpcread.getKBOs(parameters.MPCORB_FILE)
    print('Known KBOs: {}'.format(len(kbos)))
    retkbos = []
    for kbo in kbos:
        kbo.compute(date)
        # # keep only the ones that'd make it onto this plot
        if not ((extent[0] <= math.degrees(kbo.ra) <= extent[1])
                and (extent[2] <= math.degrees(kbo.dec) <= extent[3])):
            continue
        pos = (math.degrees(kbo.ra), math.degrees(kbo.dec))
        ax.scatter(pos[0],
                   pos[1],
                   marker='x',
                   facecolor='r')
        if len(kbo.name) >= 10:  # sorted out for Sofia Pro Light
            ra_shift = -0.45
        else:
            ra_shift = -0.22
        ax.annotate(kbo.name, (pos[0] + ra_shift, pos[1] + 0.06), size=7, color='r')
        retkbos.append(kbo)
    print('Retained KBOs: {}'.format(len(retkbos)))
    return ax, retkbos


def plot_known_tnos_batch(handles, labels, date):
    rate_cut = 'a > 15'
    if os.access(MPCORB, os.F_OK):
        kbos = mpcread.getKBOs(MPCORB, cond=rate_cut)
        kbo_ra = []
        kbo_dec = []
        for kbo in kbos:
            kbo.compute(ephem.date(date))
            kbo_ra.append(math.degrees(kbo.ra))
            kbo_dec.append(math.degrees(kbo.dec))
            ax.scatter(kbo_ra, kbo_dec,
                       marker='+', facecolor='none', edgecolor='g')

    return ax


def plot_single_known_tno(ax, name, date, close=[]):
    # date formatted as %Y-%m-%d %H:%M
    # FIXME: rewrite this to use astroquery.mpc's single-object retrieval.
    dateplus1hr = (datetime.datetime.strptime(date, '%Y-%m-%d %H:%M') + datetime.timedelta(1 / 24.)).strftime(
        '%Y-%m-%d %H:%M')
    obj_elems, single_ephem = horizons.batch(name, date, dateplus1hr, None, su='d')  # pull back one position only
    ra = math.degrees(ephem.degrees(ephem.hours(single_ephem[0]['RA'])))
    dec = math.degrees(ephem.degrees(single_ephem[0]['DEC']))
    if name == 'Ijiraq':
        fc = ec = 'k'
    else:
        fc = ec = 'r'
    ax.scatter(ra,
               dec,
               marker='.',
               facecolor=fc,
               edgecolor=ec,
               alpha=0.4)
    if name in close:  # keep the names from falling on top of each other
        if name == 'Hyrrokkin':
            ax.annotate(name, (ra - .07, dec + .03), size='5', color='r')
        if name == 'Ijiraq':
            ax.annotate(name, (ra - .25, dec + .03), size='5', color='k')
        if name == 'Bestla':
            ax.annotate(name, (ra - .05, dec + .03), size='5', color='r')
        if name == 'Kiviuq':
            ax.annotate(name, (ra, dec + .03), size='5', color='r')
    else:
        ax.annotate(name, (ra - 0.2, dec + .03), size='5', color='r')

    return ax


def plot_synthetic_kbos(ax, coverage):
    ra, dec, hlat, Hmag = parsers.synthetic_model_kbos(kbotype='resonant', arrays=True, maglimit=24.7)
    ax.scatter(ra, dec, c='k', marker='.', s=1, alpha=0.8)

    return ax

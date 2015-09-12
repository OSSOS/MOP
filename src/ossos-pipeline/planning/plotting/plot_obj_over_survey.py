__author__ = 'Michele Bannister   git:@mtbannister'

'''
Plot a single OSSOS object's path on the sky and observations over the course of the survey.
'''

import argparse

from matplotlib.patches import Rectangle
from matplotlib import pyplot as plt
from astropy.time import Time, TimeDelta
# import aplpy

from ossos import mpc
from ossos import orbfit
import sky_location_plots
import parameters
# import parsers
from planning.ObsStatus import query_for_observations


def plot_actual_observations(ax, obj, mark_rejected=False):
    for i, obs in enumerate(obj.mpc_observations):
        fc = 'b'
        alpha = 0.6
        marker = 'o'
        if mark_rejected:
            if obs.null_observation:
                fc = 'r'
                marker = 'x'
        ax.scatter(obs.coordinate.ra.degree, obs.coordinate.dec.degree, marker=marker, facecolor=fc, alpha=alpha)
        # is it away from the last annotation plotted? 5 days empirically worked best to remove puzzlement
        if ((obs.date - obj.mpc_observations[i - 1].date) > TimeDelta(5, format='jd')) or i == 0:
            ax.annotate(obs.date.datetime.strftime('%Y-%m-%d'),
                        (obs.coordinate.ra.degree + .5, obs.coordinate.dec.degree - 0.01),
                        size=7,
                        color='k')
    return ax


def plot_prediction(ax, obj):
    ra = []
    dec = []
    start = Time(parameters.SURVEY_START)  # min([d.date for d in obj.mpc_observations]) - TimeDelta(90, format='jd')
    end = Time('2016-01-01')
    date = start
    orbit = orbfit.Orbfit(obj.mpc_observations)
    while date < end:
        orbit.predict(date)
        ra.append(orbit.coordinate.ra.degree)
        dec.append(orbit.coordinate.dec.degree)
        date = date + TimeDelta(10, format='jd')

    ax.plot(ra, dec, 'r-', linewidth=0.6)

    # some labels for different times
    orbit.predict(start)
    ax.annotate(parameters.SURVEY_START,
                (orbit.coordinate.ra.degree - 0.1, orbit.coordinate.dec.degree),
                size=7,
                color='k')
    orbit.predict(end)
    ax.annotate(end.datetime.strftime('%Y-%m-%d'),
                (orbit.coordinate.ra.degree - 0.1, orbit.coordinate.dec.degree),
                size=7,
                color='k')

    return ax


def megacam_corresponding_to_observations(ax):
    # now while we know the KBO was in these images (here's the measurements), want the field centres
    # to make the footprints.
    discov = [m for m in obj.mpc_observations if m.discovery][0]
    obs_frames = [m.comment.frame.split('p')[0].strip(' ') for m in obj.mpc_observations]
    obs_table = query_for_observations(Time(parameters.SURVEY_START).mjd, 1, tuple(parameters.OSSOS_RUNIDS))

    camera_dimen = 0.98

    for i in range(len(obs_table) - 1, -1, -1):
        t = obs_table.data[i]
        if str(t.dataset_name) in obs_frames:
            print str(t.dataset_name)
            alpha = 0.4
            lw = 0.95
            if Time(t.StartDate, format='mjd') < Time('2014-01-01'):
                ec = 'b'
            else:
                ec = '#E47833'
                alpha = 0.5
            if str(t.dataset_name) == discov.comment.frame[0:7]:
                ec = 'k'
                lw = 1.7
            r = Rectangle(xy=(t.RA - camera_dimen / 2.0, t.DEC - camera_dimen / 2.0),
                          height=camera_dimen,
                          width=camera_dimen,
                          edgecolor=ec,
                          alpha=alpha,
                          lw=lw,
                          zorder=-100,
                          fill=False)
            ax.add_artist(r)

    return ax


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("kbo",
                        nargs='*',
                        help="Plot all OSSOS imaging of the given KBO, which can be an OSSOS discovery.")

    args = parser.parse_args()

    # load in the KBO. For now let's assume it's a single OSSOS discovery, generalise later
    # discoveries = parsers.ossos_release_parser(table=True)
    dir = '/Users/bannisterm/Dropbox/OSSOS/measure3/2014B-H/track/'
    fn = dir + args.kbo[0] + '.ast'  # parameters.REAL_KBO_AST_DIR
    obj = mpc.MPCReader(fn)  # let MPCReader's logic determine the provisional name
    # need to determine a plot extent to get the MegaCam coverage
    # and KBOs go westward so earliest to latest is kinda useful
    extents = [max([obs.coordinate.ra.degree for obs in obj.mpc_observations]) + 0.9,
               min([obs.coordinate.ra.degree for obs in obj.mpc_observations]) - 0.9,
               min([obs.coordinate.dec.degree for obs in obj.mpc_observations]),
               max([obs.coordinate.dec.degree for obs in obj.mpc_observations])]
    midpoint = (extents[0] + ((extents[1] - extents[0]) / 2.), extents[2] + ((extents[3] - extents[2]) / 2.))
    print(extents, midpoint)

    handles, labels, ax, fontP = sky_location_plots.basic_skysurvey_plot_setup()
    plt.axis('equal')
    plt.grid(alpha=0.2)
    #
    # fig = aplpy.FITSFigure()
    # fig.set_theme('publication')

    ax = plot_actual_observations(ax, obj, mark_rejected=True)
    ax = plot_prediction(ax, obj)
    ax = megacam_corresponding_to_observations(ax)

    # only for o3e13 poster child for first-quarter paper
    if args.kbo[0] == 'o3e13':
        ax.annotate('invariable plane', (213.7, -11.9), alpha=0.7, rotation=19)

    sky_location_plots.saving_closeout(args.kbo[0], '', extents, '')

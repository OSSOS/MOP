import math
import os
import cPickle

import numpy as np
from matplotlib import pyplot
import matplotlib.lines as mlines
from astropy.table import Table

from ossos import mpc
from ossos import orbfit
from planning.plotting import parsers, parameters




# parsers.parameters.RELEASE_DETECTIONS="/Users/jjk/OSSOSv4.detections"

tnos = parsers.ossos_release_parser(table=True)
symbols = {'cla': 'o',
           'cen': 'd',
           'sca': '^',
           'res': '*',
           'det': 's',
           'default': 'o'}
block_colours = {'o3e': 'r',
                 'o3o': 'b',
                 'o3l': 'k',
                 }
security_colour = {'I': 'r',  # all insecure objects are 'I' or 'IH'
                   'IH': 'r',
                   'S': 'b'}  # there's many different classifications: default to grouping them

names = ['cl', 'p', 'j', 'k', 'sh', 'object', 'mag', 'mag_uncert', 'F', 'H_sur', 'dist', 'dist_E', 'nobs',
         'time', 'av_xres', 'av_yres', 'max_x', 'max_y', 'a', 'a_E', 'e', 'e_E', 'i', 'i_E', 'node', 'node_E',
         'argperi', 'argperi_E', 'time_peri', 'time_peri_E', 'ra_dis', 'dec_dis', 'jd_dis', 'rate']
one_opp_tnos = Table.read('/Users/michele/Dropbox/OSSOS/Release_summaries/v4/OSSOSv4/OSSOSv4.detections',
                          format='ascii', guess=False,
                          delimiter=' ', data_start=0, comment='#', names=names, header_start=None)

for tno in tnos['object']:
    cl = tnos['cl'][tnos['object'] == tno]
    print tno, cl[0]

    # Check if the orbfit has already been run. Saves time as this takes a minute or so total
    pfilename = '{}_da'.format(tno)
    if os.access(pfilename, os.R_OK):
        with open(pfilename, 'r') as pf:
            (t, da) = cPickle.load(pf)
    else:
        obs = mpc.MPCReader(parameters.REAL_KBO_AST_DIR + tno + '.ast')
        mpc_obs = list(obs.mpc_observations)
        for ob in mpc_obs:
            if ob.discovery:
                t0 = ob.date.jd
                break
        # sort by absolute distance from discovery
        mpc_obs.sort(key=lambda x: math.fabs(x.date.jd - t0))

        # need minimum of 3 observations to start fitting an orbit
        t = []
        da = []
        for idx in range(3, len(mpc_obs) + 1):
            short_arc = mpc_obs[:idx]
            orb = orbfit.Orbfit(short_arc)
            short_arc.sort(key=lambda x: x.date.jd)
            arclen = short_arc[-1].date.jd - short_arc[0].date.jd
            # print len(short_arc), arclen, abs(mpc_obs[:idx][-1].date.jd - mpc_obs[:idx][0].date.jd)
            t.append(arclen / 365.25)
            da.append(orb.da / orb.a)
        with open(pfilename, 'w') as pf:
            cPickle.dump((t, da), pf)

    # change colour at the two-year point if classification has shifted from insecure to secure
    fullarc_sec = tnos['sh'][tnos['object'] == tno][0]
    one_opp_sec = one_opp_tnos['sh'][one_opp_tnos['object'] == tno][0]
    one_opp = 1.6  # want to differentiate security with one opp since discovery and security with third year obs
    one_opp_col = security_colour.get(one_opp_sec, 'b')
    fullarc_col = security_colour.get(fullarc_sec, 'b')

    # it changed classification security status...
    if one_opp_sec != fullarc_sec:
        one_opp_t = np.where(np.array(t) < one_opp)[0]
        multi_opp_t = np.where(np.array(t) >= one_opp)[0]
        print t, one_opp_t, multi_opp_t
        print len(one_opp_t), len(multi_opp_t), len(t) - 1

        pyplot.plot(one_opp_t, da[:len(one_opp_t)],
                    '-' + one_opp_col, linewidth=.1, alpha=0.2, label='_')
        pyplot.plot(one_opp_t, da[:len(one_opp_t)],
                    '.' + one_opp_col, ms=2, alpha=0.2, mec=one_opp_col, mew=0.1)
        print 'changing colour'
        pyplot.plot(multi_opp_t, da[:len(multi_opp_t)],
                    '-' + fullarc_col, linewidth=.1, alpha=0.2, label='_')
        pyplot.plot(multi_opp_t, da[:len(multi_opp_t)],
                    '.' + fullarc_col, ms=2, alpha=0.2, mec=fullarc_col, mew=0.1)
    # the classification is constant. No reason to change colour.
    else:
        pyplot.plot(t, da, '-' + fullarc_col, linewidth=.1, alpha=0.2, label='_')
        pyplot.plot(t, da, '.' + fullarc_col, ms=2, alpha=0.2, mec=fullarc_col, mew=0.1)

    # Symbol gets assigned based on final orbital classification regardless of earlier colour.
    msymbol = symbols[cl[0]]
    pyplot.plot(t[-1], da[-1], msymbol + fullarc_col, alpha=0.5, ms=4, mec='k', mew=0.1)
    # pyplot.annotate(tno, (t[-1], da[-1]), size=3)

pyplot.xlabel('arc length (years)')
pyplot.ylabel('$da/a$')
pyplot.yscale('log')
pyplot.xscale('log')
pyplot.xlim([0.02, 20])
pyplot.ylim([1e-5, 1.])
pyplot.grid(axis='y', alpha=0.6)

# Make it easy to tell where one lunation, 2 lunations, and one year are
lines = [30 / 365.0,
         60 / 365.0,
         1 - 60 / 3650]
line_names = ['30 days', '60 days', '1 year']
pyplot.vlines(lines, 1e-5, 1, alpha=0.3, linewidth=1)
for k, line in enumerate(lines):
    pyplot.annotate(line_names[k], (line, 2.8e-5), rotation='vertical', size=7,
                    xytext=(-7, 0), textcoords='offset points')

# legend had to be entirely manually hacked to get the right effect here
handler_map = [
    mlines.Line2D([], [], color='r', alpha=0.3, label='insecure'),
    mlines.Line2D([], [], color='b', alpha=0.3, label='secure'),
    # mlines.Line2D([], [], color='k', alpha=0.3, label='13BL'),
    mlines.Line2D([], [], marker='d', color='None', mec='k', alpha=0.5, linestyle=None, label='centaur'),
    mlines.Line2D([], [], marker='^', color='None', mec='k', alpha=0.5, linestyle=None, label='scattering'),
    mlines.Line2D([], [], marker='o', color='None', mec='k', alpha=0.5, linestyle=None, label='classical'),
    mlines.Line2D([], [], marker='*', color='None', mec='k', alpha=0.5, linestyle=None, label='resonant'),
    mlines.Line2D([], [], marker='s', color='None', mec='k', alpha=0.5, linestyle=None, label='detached'),
]
legend = pyplot.legend(handles=handler_map,
                       numpoints=1, loc='upper right', frameon=True, fontsize='x-small')
rect = legend.get_frame()
rect.set_facecolor('0.95')
rect.set_linewidth(0.0)

pyplot.draw()
outfile = 'OSSOS_{}_da_improvement.pdf'.format(parameters.RELEASE_VERSION)
pyplot.savefig(outfile, transparent=True, bbox_inches='tight')
print outfile

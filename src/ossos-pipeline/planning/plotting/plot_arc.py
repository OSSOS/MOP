import math
import os
import cPickle

from matplotlib import pyplot
import matplotlib.lines as mlines

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

for tno in tnos['object']:
    cl = tnos['cl'][tnos['object'] == tno]
    print tno, cl[0]

    # Check if the orbfit has already been run. Saves time as this takes a minute or so total
    pfilename = '{}_da'.format(tno)
    if os.access(pfilename,os.R_OK):
        with open(pfilename,'r') as pf:
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
            da.append(orb.da/orb.a) 
        with open(pfilename,'w') as pf:
            cPickle.dump((t, da), pf)

    msymbol = symbols[cl[0]]
    colour = 'o3e' in tno and 'r' or 'b'  # i.e. o3e are red, o3o are blue

    pyplot.plot(t, da, '-' + colour, linewidth=.1, alpha=0.2, label='_')
    pyplot.plot(t, da, '.' + colour, ms=2, alpha=0.2, mec=colour, mew=0.1)
    pyplot.plot(t[-1], da[-1], msymbol + colour, alpha=0.5, ms=4, mec='k', mew=0.1, label='_')
    # pyplot.annotate(tno, (t[-1], da[-1]), size=3)

pyplot.xlabel('arc length (years)')
pyplot.ylabel('$da/a$')
pyplot.yscale('log')
pyplot.xscale('log')
pyplot.xlim([0.02, 20])
pyplot.ylim([1e-5, 1.])
pyplot.grid(axis='y',alpha=0.6)

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
    mlines.Line2D([], [], color='r', alpha=0.3, label='13AE'),
    mlines.Line2D([], [], color='b', alpha=0.3, label='13AO'),
    mlines.Line2D([], [], marker='d', color='None', mec='k', alpha=0.5, linestyle=None, label='centaur'),
    mlines.Line2D([], [], marker='^', color='None', mec='k', alpha=0.5, linestyle=None, label='scattering'),
    mlines.Line2D([], [], marker='o', color='None', mec='k', alpha=0.5, linestyle=None, label='classical'),
    mlines.Line2D([], [], marker='*', color='None', mec='k', alpha=0.5, linestyle=None, label='resonant'),
    mlines.Line2D([], [], marker='s', color='None', mec='k', alpha=0.5, linestyle=None, label='detached'),
]
legend = pyplot.legend(handles=handler_map, numpoints=1, loc='upper right', frameon=True, fontsize='x-small')
rect = legend.get_frame()
rect.set_facecolor('0.95')
rect.set_linewidth(0.0)

pyplot.draw()
outfile = 'OSSOS_{}_da_improvement.pdf'.format(parameters.RELEASE_VERSION)
pyplot.savefig(outfile, transparent=True, bbox_inches='tight')
print outfile

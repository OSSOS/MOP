

__author__ = 'bannisterm'

import pandas
import math
import numpy as np
import palettable
from matplotlib import pyplot as plt
import matplotlib.lines as mlines
from astropy.table import Table

from src.ossos.utils import parsers

palette = palettable.tableau.ColorBlind_10.mpl_colors

def parse_nate_sims(path):
    '''
    parts0.dat) contains the id number, particle fraction (ignore) a, ecc, inc, long. asc., arg. per, and mean anomaly
    for every particle in the simulation at t=0.
    The second (parts3999.dat) contains the same info at t=3.999 Gyrs for these particles.

    :return:
    '''
    zerostate = pandas.read_table(path + 'parts0.dat', delim_whitespace=True)
    endstate = pandas.read_table(path + 'parts3999.dat', delim_whitespace=True)

    # add perihelion
    zerostate['q'] = zerostate['a'] * (1 - zerostate['e'])
    endstate['q'] = endstate['a'] * (1 - endstate['e'])

    return zerostate, endstate


def diffusion(q, t_initial=4*10**9):
    '''
    Valid for i = 0-30 deg.
    '''
    a = 1 / ( t_initial**2 * (10**(-4.8 - ((0.3/5)*(q-32))))**4 )
    return a


def plot_tnos(ax, name=True, ossos=True):
    # plot the star of the show first
    plt.errorbar(740, 50, xerr=70,
                 fmt='*', ms=12,
                 mfc='r', mec=palette[3], ecolor='r', elinewidth=1, capsize=0, zorder=0,
                 label='_nolegend_')
    plt.text(630, 50.5, 'L91', fontsize='small')

    tnos = Table.read('/Users/bannisterm/Dropbox/Papers_in_progress/OSSOS/high_q_uo3l91/data/all_tnos_no_centaurs.csv',
                      format='csv', data_start=1)
    plt.errorbar(tnos['a'], tnos['q'],
                 xerr=tnos['sigma_a'],
                 fmt='o', ms=6,
                 mfc=palette[3], mec=palette[3], ecolor=palette[3], capsize=0, elinewidth=0.5, zorder=0,
                 label='_nolegend_')

    if name:
        for id in ['GP136', 'Sedna', 'VP113', 'GB174', 'SR349', 'VN112', 'CR105', 'FT28', 'FE72']:
            for tno in tnos:
                if tno['full_name'].__contains__(id):
                    xoffset = 15
                    yoffset = 0.5
                    if id in ['SR349']: # pesky overlapping labels
                        xoffset = -55
                    if id in ['VN112']:
                        yoffset = -0.2
                    plt.text(tno['a']+xoffset, tno['q']+yoffset, id, fontsize='xx-small')

    if ossos:
        ossos_tnos = parsers.ossos_release_parser(table=True)
        ossos_tnos['q'] = ossos_tnos['a'] * (1. - ossos_tnos['e'])
        ossos_tnos['e_q'] = (ossos_tnos['e_a'] / ossos_tnos['a']) + (ossos_tnos['e_e'] / ossos_tnos['e'])

        plt.errorbar(ossos_tnos['a'], ossos_tnos['q'],
                     xerr=ossos_tnos['e_a'],
                     yerr=ossos_tnos['e_q'],
                     fmt='o', ms=6,
                     capsize=0, elinewidth=0.5, mfc=palette[2],
                     mec=palette[3], ecolor=palette[2], zorder=1,
                     label='_nolegend_'
                     )
        for tno in ossos_tnos:
            if tno['object'] in ['o5t52', 'o5m52', 'o5p060', 'o3l83', 'o5m72']:
                plt.text(tno['a'] + 5, tno['q'] + 0.3, tno['object'], fontsize='xx-small')
    return


def plot_aq(data, path, plot_ossos):
    xmin = 70
    xmax = 3000
    ymin = 30
    ymax = 85

    plt.figure(figsize=(8, 7))
    # a_initial, q_initial
    d = data.sample(n=3000)  # plot only a randomised subsample to reduce plot size (otherwise goes >1 MB)
    plt.semilogx(d.a_start, d.q_start,
                 color=palette[1], alpha=0.4,
                 marker='.', ms=3, linestyle='None', zorder=0,
                 label='initial state')

    # a_final, q_final
    astep = [0.25, 1., 1.]
    # astep = [n/4. for n in astep]
    # astep.append(50)
    print(astep)
    print(len(data))
    p = palettable.cubehelix.cubehelix1_16.mpl_colors[-7::-2]

    data['da'] = abs((data['a_end']-data['a_start']))/data['a_start']
    label = ''
    for n, da in enumerate(astep):
        # plot where objects are within 2*a of their initial a to map out the boundary
        if n == 0:
            print((n, da))
            particles = data[data['da'] <= da]
            # particles = data.query('(a_end/a_start < @da)')  # improved from a_end <= @da*a_start
            print((len(particles), particles['da']))
            label = '$\delta a/a \leq {}$'.format(da)
        elif n > 0 and n < len(astep)-1:
            print((n, da))
            particles = data[(astep[n-1] < data['da']) & (data['da'] <= da)]   #data.query('(@astep[@n-1] < a_end/a_start <= @da)')
            print((len(particles), particles['da']))
            label = '${} < \delta a/a \leq {}$'.format(astep[n-1], da)
        else:
            print((n, da))
            particles = data[data['da'] > da]   #data.query('(a_end/a_start > @da)')
            print((len(particles), particles['da']))
            label = '$\delta a/a > {}$'.format(da)

        plt.semilogx(particles.a_end, particles.q_end,
                     color=p[n], alpha=1,
                     marker='.', ms=4, linestyle='None', zorder=n+1,
                     label=label
                     )

    diff_eqn = [diffusion(q) for q in range(ymin, ymax)]
    plt.plot(diff_eqn, range(ymin, ymax), linewidth=2, alpha=0.4, color=palette[0], zorder=0)

    plot_tnos(plt.gca(), ossos=plot_ossos)

    plt.ylim(ymin, ymax)
    plt.xlim(xmin, xmax)

    # indicate Neptune's orbit
    # plt.axhspan(ymin, 30, facecolor='k', alpha=0.2)
    # plt.text(6*(10**2), 27, "Neptune-crossing", fontsize="small", alpha=0.9)

    # shade region 80-100 AU and label as 'assumptions invalid'
    plt.axvspan(xmin, 100, facecolor='k', alpha=0.1)
    plt.text(80, 70, 'assumptions invalid', rotation=90, alpha=0.5)
    # label the conditions that are dominant in each region
    plt.text(250, 33, 'strong mobility', alpha=0.4, fontsize='large')
    plt.text(300, 53, 'diffusion', alpha=0.4, fontsize='large')

    # show the 11:1 Neptune resonance
    a_N = 30.06896348
    a_res = a_N * (float(11) / float(1)) ** (2 / 3.)
    ecc = np.arange(100)
    ecc = ecc/100. #[n/100. for n in ecc]
    peri = [a_res*(1-e) for e in ecc]
    plt.plot(peri, a_res**np.ones(100), 'k', alpha=0.4)

    handles, labels = plt.gca().get_legend_handles_labels()
    mpc_symbol = mlines.Line2D([], [], marker='o', color=palette[3], linestyle='None', markersize=1.5)
    ossos_symbol = mlines.Line2D([], [], marker='o', color=palette[2], linestyle='None', markersize=1.5)
    if plot_ossos:
        plt.legend(handles+[mpc_symbol, ossos_symbol], labels+['known TNOs', 'OSSOS TNOs'], loc='best', numpoints=1, fontsize='x-small', markerscale=5)
    else:
        plt.legend(handles+[mpc_symbol], labels+['known TNOs'], loc='best', numpoints=1, fontsize='x-small', markerscale=5)

    plt.xlabel('semimajor axis (au)')
    plt.ylabel('perihelion (au)')
    plt.grid(True, which='both', alpha=0.2)
    plt.draw()

    fn = 'diffusion'
    if plot_ossos:
        fn = fn + '_wOSSOS'
    outfile = path + fn + '.pdf'
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


if __name__ == '__main__':
    path = '/Users/bannisterm/Dropbox/Papers_in_progress/OSSOS/high_q_uo3l91/'

    start, end = parse_nate_sims(path+'data/')
    survivors_at_start = start[start.id.isin(end.id)]
    # print len(start), len(end) : 10,000 start, 8685 survive (others lost due to scattering).
    assert len(survivors_at_start) == len(end)

    # I want the rows where for the same particle ID, the a did not change by more than 2x the initial a.
    result = pandas.merge(survivors_at_start, end, on='id', suffixes=['_start', '_end'])
    # print len(two_a): 7656 particles.

    print('i_start 1<6', len([ math.degrees(n) for n in result[result['inc_start'] < math.radians(6.)]['inc_start'] ]))

    # print 'i_end range', math.degrees(min(result['inc_end'])), math.degrees(max(result['inc_end']))

    k = [ math.degrees(n) for n in result[(result['a_end'] < 810.) & (result['q_end'] > 45.) & (result['q_end'] < 55.)]['inc_end'] ]
    k.sort()
    print(('a_end < 810. and its inclinations',
        len(result[(result['a_end'] < 810.) & (result['q_end'] > 45.)  & (result['q_end'] < 55.)]),
          k
          ))
    # print [math.degrees(n) for n in result[result['a_end'] < 800.]['inc_end']]
    print('a_end < 810 and i < 26', len(result[(result['a_end'] < 810.) & (result['inc_end'] > math.radians(154.))]))
    # print [math.degrees(n) for n in result[(result['a_end'] < 810.) & (result['inc_end'] > math.radians(154.))]['inc_end']]


    # print 'a_end < 820, a', [n for n in result[result['a_end'] < 800.]['a_end']]

    # plot_aq(result, path+'figures/', False)
from __future__ import absolute_import

__author__ = 'bannisterm'

import pandas
import palettable
from matplotlib import pyplot as plt

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


def plot_aq(data, path):
    xmin = 70
    ymin = 30
    ymax = 500

    plt.figure()
    # a_initial, q_initial
    plt.semilogx(data.a_start, data.q_start,
                 color='r', alpha=0.1,
                 marker='.', ms=2, linestyle='None', zorder=0,
                 label='initial state')
    # a_final, q_final
    astep = range(10)
    astep = [n/4. for n in astep]
    astep.append(50)
    for n, da in enumerate(astep):
        print n, da
        particles = data.query('a_end <= @da*a_start')
        print(da, len(particles))
        plt.semilogx(particles.a_end, particles.q_end,
                     color='b', alpha=0.3,
                     marker='.', ms=2, linestyle='None', zorder=1,
                     label='after 4 Gyr')

    diff_eqn = [diffusion(q) for q in xrange(ymin, ymax)]
    plt.plot(diff_eqn, xrange(ymin, ymax))

    plt.ylim(ymin, ymax)
    plt.xlim(xmin, 10**4)

    # shade region 80-100 AU and label as 'assumptions invalid'
    exclude = plt.Rectangle((xmin, ymin), 100-xmin, ymax-ymin, facecolor='k', alpha=0.1)
    plt.gca().add_patch(exclude)
    plt.text(80, 46, 'assumptions invalid', rotation=90, alpha=0.5)
    # label conditions dominant in each region
    plt.text(10**3, 37, 'strong mobility', alpha=0.5)
    plt.text(2*10**2, 53, 'diffusion', alpha=0.5)

    plt.legend(loc='lower right', numpoints=1, fontsize='small')
    plt.xlabel('semimajor axis (AU)')
    plt.ylabel('perihelion (AU)')
    plt.grid(alpha=0.2)
    plt.draw()
    plt.show()
    outfile = path + 'diffusion.pdf'
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


if __name__ == '__main__':
    path = '/Users/bannisterm/Dropbox/Papers_in_progress/OSSOS/high_q_uo3l91/data/'

    start, end = parse_nate_sims(path)
    survivors_at_start = start[start.id.isin(end.id)]
    # print len(start), len(end) : 10,000 start, 8685 survive (others lost due to scattering).
    assert len(survivors_at_start) == len(end)

    # I want the rows where for the same particle ID, the a did not change by more than 2x the initial a.
    result = pandas.merge(survivors_at_start, end, on='id', suffixes=['_start', '_end'])
    # print len(two_a): 7656 particles.

    plot_aq(result, path)
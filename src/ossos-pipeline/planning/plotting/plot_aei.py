__author__ = 'Michele Bannister   git:@mtbannister'

import matplotlib.pyplot as plt
import numpy

import parsers
import parameters
import plot_fanciness


def full_aei(data):
    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.05)
    ms = 3
    alpha = 0.4
    grid_alpha = 0.3
    ecolor = '0.1'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'

    ax[0].errorbar(data['dist'], data['i'],
                   xerr=data['dist_E'], yerr=data['i_E'],
                   fmt=fmt, alpha=alpha, ecolor=ecolor, capsize=capsize, ms=ms)
    ax[0].set_ylabel('inclination (degrees)')
    ax[0].set_ylim([-1, 55])  # Defaults to appropriate for v4 release. May want different in future...
    ax[0].grid(True, alpha=grid_alpha)

    ax[0].set_xlabel('heliocentric distance (AU)')

    ax[1].errorbar(data['a'], data['i'],
                   xerr=data['a_E'], yerr=data['i_E'],
                   fmt=fmt, alpha=alpha, ecolor=ecolor, capsize=capsize, ms=ms)
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].set_ylim([-1, 55])  # Defaults to appropriate for v4 release. May want different in future...
    ax[1].grid(True, alpha=grid_alpha)

    ax[2].errorbar(data['a'], data['e'],
                   xerr=data['a_E'], yerr=data['e_E'],
                   fmt=fmt, alpha=alpha, ecolor=ecolor, capsize=capsize, ms=ms)
    ax[2].set_ylabel('eccentricity')
    ax[2].set_ylim([-0.001, .8])
    ax[2].grid(True, alpha=grid_alpha)

    plt.xlim([10, 155])  # Defaults to appropriate for v4 release. May want different in future...

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])

    plt.xlabel('semimajor axis (AU)')

    plt.draw()
    outfile = 'OSSOS_v{}_discoveries_aei.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


def classicals_aei(data):
    # designed to match the parameters and look of Petit et al. 2011
    # layout: top left, a-q, top right, i-q, lower left, a-i.
    peri = data['a'] * (1. - data['e'])

    ms = 5
    capsize = 2  # error bar cap width

    fig, ax = plt.subplots(2, 2, figsize=(10, 10))
    fig.subplots_adjust(hspace=0.0)  # want to work on the v-space as well

    ax[0][0].errorbar(data['a'], peri,
                      xerr=data['a_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', capsize=capsize, ms=ms)
    ax[0][0].grid(True, alpha=0.4)
    ax[0][0].set_xlim([39., 48.])
    ax[1][0].set_xticks(range(40, 49, 2))
    ax[0][0].set_ylim([34., 48.])
    ax[0][0].set_ylabel('pericenter (AU)')
    ax[0][0].set_yticks(range(36, 50, 4))

    ax[0][1].errorbar(data['i'], peri,
                      xerr=data['i_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', capsize=capsize, ms=ms)
    ax[0][1].grid(True, alpha=0.4)
    ax[0][1].set_xlim([0., 35.])
    ax[0][1].set_xlabel('inclination (deg)')
    ax[0][1].set_xticks(range(5, 40, 10))
    ax[0][1].set_ylim([34., 48.])
    ax[0][1].set_yticks(range(36, 50, 4))

    ax[1][0].errorbar(data['a'], data['i'],
                      xerr=data['a_E'], yerr=data['i_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', ms=ms, capsize=capsize)
    ax[1][0].set_xlim([39., 48.])
    ax[1][0].set_xticks(range(40, 49, 2))
    ax[1][0].set_xlabel('semimajor axis (AU)')
    ax[1][0].set_ylim([0., 35.])
    ax[1][0].set_ylabel('inclination (deg)')
    ax[1][0].set_yticks(range(0, 40, 10))
    ax[1][0].grid(True, which='both', alpha=0.4)

    plot_fanciness.remove_border(ax[0][0])
    ax[0][0].tick_params(labelbottom='off')
    plot_fanciness.remove_border(ax[0][1])
    plot_fanciness.remove_border(ax[1][0])
    plot_fanciness.remove_border(ax[1][1], remove=['left', 'right', 'top', 'bottom'], keep=[])
    ax[1][1].tick_params(labelbottom='off', labelleft='off')

    plt.draw()
    outfile = 'OSSOS_v{}_classical_aei.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


if __name__ == '__main__':
    tnos = parsers.ossos_release_parser(table=True)  # return as an astropy.Table.Table

    # full_aei(tnos)
    classicals = tnos[numpy.array([name.startswith('m') for name in tnos['p']])]
    classicals_aei(classicals)

__author__ = 'Michele Bannister   git:@mtbannister'

import sys

import matplotlib.pyplot as plt
import numpy

import palettable.tableau as pt
import parsers
import parameters
import plot_fanciness


def full_aei(data, icut=False, aiq=False):
    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.05)

    data['peri'] = data['a'] * (1. - data['e'])
    data['peri_E'] = (data['a_E'] / data['a']) + (data['e_E'] / data['e'])

    # Appropriate for v4 release. May want different in future...
    if parameters.RELEASE_VERSION == 4:
        ymin = -0.001
        imax = 55
        emax = .65
        qmin = data['peri'].min() - 4
        qmax = data['peri'].max() + 4
        xinner = 25
        xouter = 90
        annotation = False

    col = pt.PurpleGray_6.mpl_colors
    coldcol = 'b'  # col[3]
    hotcol = col[2]
    e45col = 'r'  # coldcol
    ms = 8
    cold_alpha = 0.35  # when plotting blue only  # 0.7
    hot_alpha = 0.25
    grid_alpha = 0.2
    ebarcolor = '0.1'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'

    sans_o3e45 = data[numpy.where(data['object'] != 'o3e45')]
    if icut:
        cold = sans_o3e45[numpy.where(sans_o3e45['i'] < 5.)]
        hot = sans_o3e45[numpy.where(sans_o3e45['i'] >= 5.)]
    else:  # no splits whatsoever
        cold = sans_o3e45

    # want to show o3e45 with a different symbol.
    o3e45 = data[numpy.where(data['object'] == 'o3e45')]

    ax = helio_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, annotation=annotation)
    if icut:
        ax = helio_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, annotation=annotation)
    ax = helio_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, annotation=True)
    ax[0].set_ylim([ymin, imax])

    ax = a_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, annotation=annotation)
    if icut:
        ax = a_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, annotation=annotation)
    ax = a_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, annotation=True)
    ax[1].set_ylim([ymin, imax])

    if aiq:
        ax = a_q(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, '$i < 5^{\circ}$',
                 1, annotation=annotation)
        if icut:
            ax = a_q(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, annotation=annotation)
        ax = a_q(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, annotation=True)
        ax[2].set_ylim([qmin, qmax])
    else:
        ax = a_e(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, '$i < 5^{\circ}$',
                 1, annotation=annotation)
        if icut:
            ax = a_e(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, annotation=annotation)
        ax = a_e(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, annotation=True)
        ax[2].set_ylim([ymin, emax])

    plt.xlim([xinner, xouter])
    ax[0].set_xticks(range(xinner, xouter, 5))
    ax[1].set_xticks(range(xinner, xouter, 5))
    ax[2].set_xticks(range(xinner, xouter, 5))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])

    plt.xlabel('semimajor axis (AU)')
    if icut:
        plt.legend(loc='lower right', numpoints=1, fontsize='small')

    plt.draw()
    if aiq:
        type = 'aiq'
    else:
        type = 'aei'
    outfile = 'OSSOS_v{}_discoveries_{}_{}-{}_ilabel.pdf'.format(parameters.RELEASE_VERSION, type, xinner, xouter)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')
    sys.stdout.write('{}\n'.format(outfile))


def helio_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, annotation=False):
    ax[0].errorbar(data['dist'], data['i'],
                   xerr=data['dist_E'], yerr=data['i_E'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour)
    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)
    ax[0].set_xlabel('heliocentric distance (AU)')

    if annotation:
        for obj in data:
            ax[0].annotate(obj['object'], (obj['dist'] + 0.7, obj['i']), size=5)

    return ax


def a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, annotation=False):
    ax[1].errorbar(data['a'], data['i'],
                   xerr=data['a_E'], yerr=data['i_E'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour)
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[1].annotate(obj['object'], (obj['a'] + 0.7, obj['i']), size=5)

    return ax


def a_e(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, annotation=False):
    ax[2].errorbar(data['a'], data['e'],
                   xerr=data['a_E'], yerr=data['e_E'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour,
                   label=label)
    ax[2].set_ylabel('eccentricity')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['e']), size=5)

    return ax


def a_q(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, annotation=False):
    ax[2].errorbar(data['a'], data['peri'],
                   xerr=data['a_E'], yerr=data['peri_E'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour,
                   label=label)
    ax[2].set_ylabel('perihelion (AU)')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['peri']), size=5)

    return ax


def classicals_qi(data):
    # the top right panel i-q of Petit et al. only.
    data['peri'] = data['a'] * (1. - data['e'])
    data['peri_E'] = (data['a_E'] / data['a']) + (data['e_E'] / data['e'])

    o3e45 = data[numpy.where(data['object'] == 'o3e45')]
    classicals = data[numpy.where(data['p'] == 'm')]

    ms = 11
    capsize = 1  # error bar cap width
    alpha = 0.3
    fig, ax = plt.subplots(1, 1, figsize=(5, 5))
    ax.errorbar(classicals['i'], classicals['peri'],
                xerr=classicals['i_E'],
                yerr=classicals['peri_E'],
                fmt='.', alpha=alpha, ecolor='0.1', capsize=capsize, ms=ms)
    ax.errorbar(o3e45['i'], o3e45['peri'],
                xerr=o3e45['i_E'],
                yerr=o3e45['peri_E'],
                fmt='*', alpha=0.7, ecolor='0.1', capsize=capsize, ms=ms, color='r')

    ax.grid(True, alpha=0.4)
    ax.set_xlim([0., 35.])
    ax.set_xlabel('inclination (deg)')
    ax.set_xticks(range(5, 40, 10))
    ax.set_ylim([34., 48.])
    ax.set_yticks(range(36, 50, 4))
    ax.set_ylabel('perihelion (AU)')
    plot_fanciness.remove_border(ax)

    plt.draw()
    outfile = 'OSSOS_v{}_classical_iq.pdf'.format(parameters.RELEASE_VERSION)
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

    full_aei(tnos)
    # classicals_qi(tnos)

#     classicals = tnos[numpy.array([name.startswith('m') for name in tnos['p']])]
#     classicals_aei(classicals)

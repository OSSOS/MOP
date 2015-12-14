__author__ = 'Michele Bannister   git:@mtbannister'

import sys

import matplotlib.pyplot as plt
import numpy
from ossos import storage
import palettable

def full_aei(data, icut=3.5, aiq=False, release='current', high_light=[]):

    fig, ax = plt.subplots(2, 2, figsize=(14, 16))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.15)
    fig.suptitle(release+" diagnostic plots")
    orbit_classes = ['cla', 'res', 'sca', 'det', 'cen']

    xinner = 15
    xouter = 90
    annotation = False

    data['peri'] = data['a'] * (1. - data['e'])
    data['peri.E'] = data['peri']*((data['a.E'] / data['a']) + (data['e.E'] / data['e']))

    data.sort('cl')

    col = palettable.wesanderson.Zissou_5.mpl_colors[0:3] + [(0.0, 0.6, 0.4)] + [(1.0, 0.0, 0.0)]

    coldcol = col
    ms = 8
    cold_alpha = 0.8  # when plotting blue only  # 0.7
    grid_alpha = 0.2
    ebarcolor = 'k'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'

    cold = data

    a_q(ax[1][0], cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes,
        annotation=False)

    a_i(ax[1][1], cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes,
        annotation=False)

    arc_da(ax[0][1], cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol,
              '$i < {}^{}$'.format(icut,'{\circ}'), 1, orbit_classes, annotation=annotation)

    d_da(ax[0][0], cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha,
         coldcol, '$i < {}^{}$'.format(icut, '{\circ}'), 1, orbit_classes, annotation=annotation)

    plt.draw()
    outfile = '{}ScatterPlots.pdf'.format(release, type, xinner, xouter)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')
    sys.stdout.write('{}\n'.format(outfile))


def a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        ax.errorbar(tnos['a'], tnos['i'],
                    xerr=tnos['a.E'], yerr=tnos['i.E'],
                    zorder=zorder,
                    fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms,
                    label=orbclass,
                    mfc=colour[i], mec=colour[i],
                    mew=0.5)
    ax.set_ylabel('inclination (degrees)')
    ax.set_xlabel('semi-major axis (AU)')
    ax.legend()
    ax.grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax.annotate(obj['object'], (obj['a'] + 0.7, obj['i']), size=5)


def arc_da(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[data['cl']==orbclass]
        label = orbclass
        ax.errorbar(tnos['time'], tnos['a.E']/tnos['a'],
                       xerr=None, yerr=None,
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5,
                       label=label)

    ax.set_yscale('log')
    ax.set_xscale('log')
    ax.set_xlabel('log(arc-length in years)')
    ax.set_ylabel('log(da/a)')
    ax.grid(True, alpha=grid_alpha)

def d_da(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[data['cl']==orbclass]
        label = orbclass
        ax.errorbar(tnos['dist'], tnos['a.E']/tnos['a'],
                       xerr=tnos['d.E'], yerr=None,
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5,
                       label=label)
    ax.set_yscale('log')
    ax.grid(True, alpha=grid_alpha)
    ax.set_ylabel('log(da/a)')
    ax.set_xlabel('heliocentric distance (AU)')


def a_q(ax,
        data,
        fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour,
        zorder,
        orbit_classes,
        annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[data['cl'] == orbclass]
        label = orbclass
        ax.errorbar(tnos['a'], tnos['peri'],
                    xerr=tnos['a.E'], yerr=None,
                    zorder=zorder,
                    fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i],
                    mew=0.5,
                    label=label)
    ax.set_ylabel('perihelion (AU)')
    ax.set_xlabel('semi-major axis (AU)')
    ax.grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax.annotate(obj['object'], (obj['a'] + 0.7, obj['peri']), size=5)


if __name__ == '__main__':
    release = len(sys.argv) > 1 and sys.argv[1] or 'current'

    tnos = storage.get_detections()

    full_aei(tnos, release=release)

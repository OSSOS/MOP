
__author__ = 'Michele Bannister   git:@mtbannister'

import sys
import numpy

import matplotlib.pyplot as plt
#import matplotlib.lines as mlines
from astropy import units
from astropy.table import Table

import palettable
from ossos import (parameters)
from src.ossos.utils import parsers
from src.ossos.planning.plotting import plot_fanciness


def full_aei(data_release, fov, icut=False, aiq=False):
    fig, ax = plt.subplots(2, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.15)

    ymin = -0.001
    orbit_classes = ['cla', 'res', 'sca', 'det']#, 'xxx']  # fewer unclassified objects now
    colmap = palettable.wesanderson.Zissou_5.mpl_colors
    col = colmap[0:4] #['m'] + [colmap[4]] +

    data = data_release[numpy.where(data_release['data_arc'] > 365.)]
    data = data[numpy.where(data['sigma_a'] < 100.)]
    print(len(data))

    if fov == 'medium':
        imax = 55
        emax = 0.86
        xinner = 30
        xouter = 160
        xstep = 10
        qmin = 30
        qmax = 90
        ms = 4
        alpha = 0.7  # when plotting blue only  # 0.7
        annotation = False
        plot_resonances = True

    if parameters.RELEASE_VERSION == '9' and fov == 'classical':
        imax = 45
        emax = 0.53  #0.37
        xinner = 30 #38
        xouter = 60 #49
        xstep = 5
        ms = 5
        alpha = 0.65
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        data = data[numpy.array([n < .15 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        data = data[numpy.array([n < .02 for n in data['e_e']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if fov == 'thousand_au':
        imax = 40
        emax = 1.0
        xinner = 0
        xouter = 850
        xstep = 50
        ms = 9
        alpha = 0.85
        annotation = False
        plot_resonances = True

    coldcol = col
    hotcol = col[2]
    hot_alpha = 0.25
    grid_alpha = 0.2
    ebarcolor = 'k'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'
    alpha = alpha

    ax = a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes, annotation=annotation)
    ax[0].set_ylim([ymin, imax])

    if aiq:
        ax = a_q(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, '$i < 5^{\circ}$',
                 1, annotation=annotation)
        ax[1].set_ylim([qmin, qmax])
    else:
        ax = a_e(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 'placeholder', 1, orbit_classes,
             annotation=annotation)

        # constant q line
        e = [(1-(40./a)) for a in range(xinner+1, xouter, 3)]
        ax[1].plot(list(range(xinner+1, xouter, 3)), e, '-')

        ax[1].set_ylim([ymin, emax])

    plt.xlim([xinner, xouter])
    ax[0].set_xticks(list(range(xinner, xouter, xstep)))
    ax[1].set_xticks(list(range(xinner, xouter, xstep)))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])

    if plot_resonances:
        resonances(ax, imax, emax)

    handles, labels = ax[1].get_legend_handles_labels()
    # handles.append(mlines.Line2D([], [], marker='*', color=col[0], alpha=cold_alpha, linestyle=None))
    labels = ['q < 40 au', 'q >= 40 au']
    ax[1].legend(handles, labels, loc='lower right', numpoints=1, fontsize='small')

    plt.xlabel('semimajor axis (AU)')

    plt.draw()
    if aiq:
        type = 'aiq'
    else:
        type = 'aei'
    outfile = 'mpcTNOs_{}_{}-{}au.pdf'.format(type, xinner, xouter)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')
    sys.stdout.write('{}\n'.format(outfile))


# def helio_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
#     for i, orbclass in enumerate(orbit_classes):
#         print i, orbclass
#         tnos = data[numpy.where(data['cl'] == orbclass)]
#
#         ax[0].errorbar(tnos['dist'], tnos['i'],
#                        # xerr=tnos['e_dist'], yerr=tnos['e_i'],
#                        zorder=zorder,
#                        fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i],
#                        mec=colour[i], mew=0.5)
#
#     ax[0].set_ylabel('inclination (degrees)')
#     ax[0].grid(True, alpha=grid_alpha)
#     ax[0].set_xlabel('heliocentric distance (AU)')
#
#     if annotation:
#         for obj in data:
#             ax[0].annotate(obj['object'], (obj['dist'] + 0.7, obj['i']), size=5)
#
#     return ax


def a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    scattering = data[numpy.where(data['q'] < 40.)]
    detached = data[numpy.where(data['q'] >= 40.)]
    for i, tnos in enumerate([scattering, detached]):
        ax[0].errorbar(tnos['a'], tnos['i'],
                       xerr=tnos['sigma_a'], yerr=tnos['sigma_i'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5)
    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[1].annotate(obj['object'], (obj['a'] + 0.7, obj['i']), size=5)

    return ax


def a_e(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, orbit_classes, annotation=False):
    scattering = data[numpy.where(data['q'] < 40.)]
    detached = data[numpy.where(data['q'] >= 40.)]
    for i, tnos in enumerate([scattering, detached]):
        ax[1].errorbar(tnos['a'], tnos['e'],
                       xerr=tnos['sigma_a'], yerr=tnos['sigma_e'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5,
                       )
    ax[1].set_ylabel('eccentricity')
    ax[1].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[1].annotate(obj['object'], (obj['a'] + 0.7, obj['e']), size=5)

    return ax


def a_q(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, annotation=False):
    ax[2].errorbar(data['a'], data['peri'],
                   xerr=data['e_a'], yerr=data['e_peri'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour,
                   label=label)
    ax[2].set_ylabel('perihelion (AU)')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['peri']), size=5)

    return ax


def resonances(ax, imax, emax):
    strong_ids = ['1:1', '2:1', '3:1', '4:1', '5:1', '6:1', '7:1', '8:1', '9:1', '10:1', '11:1',
               '3:2', '5:2', '7:2', '9:2', '11:2',
               ]
    weak_ids = [
               '4:3', '5:3', '7:3', '8:3', #'11:3',
               '5:4', '7:4',
               # '9:5',
               #'11:6',
               '12:5', #'12:7',
               '13:6', #'13:7',
               # '15:8',
               '16:3', #'16:9',
               '17:9', #'17:7',
               #'19:10',
               ]
    i_lower = [0, 3]
    e_lower = [0, 0.05]
    textsize = ['xx-small', 5]
    a_N = 30.06896348
    for k, res_ids in enumerate([strong_ids, weak_ids]):
        for res in res_ids:
            m, n = res.split(':')
            a_res = a_N * (float(m)/float(n))**(2/3.)
            ax[0].axvline(a_res, alpha=0.15, zorder=0)
            ax[0].annotate(res, (a_res, imax-i_lower[k]), rotation=90, ha='center', size=textsize[k], alpha=0.7)
            ax[1].axvline(a_res, alpha=0.15, zorder=0)
            ax[1].annotate(res, (a_res, emax-e_lower[k]), rotation=90, ha='center', size=textsize[k], alpha=0.7)
    a_U = 19.18916464
    ax[0].axvline(a_U, alpha=0.15, zorder=0)
    ax[0].annotate('U 1:1', (a_U, imax), rotation=90, ha='center', size='xx-small', alpha=0.7)
    ax[1].axvline(a_U, alpha=0.15, zorder=0)
    ax[1].annotate('U 1:1', (a_U, emax), rotation=90, ha='center', size='xx-small', alpha=0.7)

    return


def argperi_a(directory):
    # dir = '/Users/bannisterm/Dropbox/OSSOS/measure3/test_argperi_align/'
    # directory = '/Users/bannisterm/Dropbox/OSSOS/measure3/2014B-H/track/'
    tnos = parsers.ossos_discoveries(directory=directory, all_objects=True)

    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.25)

    for obj in tnos:
        if obj.orbit.arc_length < 60.*units.day:
            print('Skipping', obj.name, obj.orbit.arc_length)
            continue

        alpha = 0.4

        obj_r = obj.orbit.distance.value
        obj_dr = obj.orbit.distance_uncertainty.value
        obj_a = obj.orbit.a.value
        obj_da = obj.orbit.da.value
        obj_i = obj.orbit.inc.value
        obj_di = obj.orbit.dinc.value
        obj_e = obj.orbit.e.value
        obj_de = obj.orbit.de.value

        obj_peri = obj_a * (1. - obj_e)
        obj_dperi = (obj_da/obj_a) + (obj_de/obj_e)
        obj_argP = obj.orbit.om.value
        if obj_argP > 180.:
            obj_argP = obj_argP - 360.
        obj_dargP = obj.orbit.dom.value

        ax[0].errorbar(obj_r, obj_i,
                       xerr=obj_dr,
                       yerr=obj_di,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        ax[1].errorbar(obj_a, obj_i,
                       xerr=obj_da,
                       yerr=obj_di,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        ax[2].errorbar(obj_a, obj_e,
                       xerr=obj_da,
                       yerr=obj_de,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        # ax[3].errorbar(obj_a, obj_argP,
        #                xerr=obj_dargP,
        #                yerr=obj_da,
        #                fmt='.', ms=10, color='b', alpha=alpha
        #                )

    ymin = -0.001
    imax = 50
    emax = .85
    xinner = 20
    xouter = 80
    xticker = 5
    grid_alpha = 0.2

    resonances(ax, imax, emax)

    ax[0].set_ylim([ymin, imax])
    ax[1].set_ylim([ymin, imax])
    ax[2].set_ylim([ymin, emax])
    # ax[3].set_ylim([-180, 180])
    plt.xlim([xinner, xouter])


    ax[0].set_xticks(list(range(xinner, xouter, xticker)))
    ax[1].set_xticks(list(range(xinner, xouter, xticker)))
    ax[2].set_xticks(list(range(xinner, xouter, xticker)))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])
    # plot_fanciness.remove_border(ax[3])

    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)
    ax[0].set_xlabel('heliocentric distance (AU)')
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].grid(True, alpha=grid_alpha)
    ax[2].set_ylabel('eccentricity')
    ax[2].grid(True, alpha=grid_alpha)
    # ax[2].set_xlabel('semimajor axis (AU)')
    plt.xlabel('semimajor axis (AU)')
    # ax[3].grid(True, alpha=grid_alpha)
    # ax[3].set_xlabel('perihelion (AU)')
    # ax[3].set_ylabel('arg. peri. (degrees)')

    plt.draw()
    # ofile = 'OSSOS+CFEPS+NGVS_aei_argperi.pdf'
    outfile = 'OSSOS_aei_{}.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


if __name__ == '__main__':
    # tnos = parsers.ossos_release_parser(table=True)  # return as an astropy.Table.Table

    tnos = Table.read('/Users/bannisterm/Dropbox/Papers_in_progress/OSSOS/high_q_uo3l91/data/all_tnos_no_centaurs.csv',
                      format='csv', data_start=1)

    # argperi_a(parameters.REAL_KBO_AST_DIR)

    fov = ['medium', 'classical', 'thousand_au']
    full_aei(tnos, fov[2])
    # classicals_qi(tnos)

#     classicals = tnos[numpy.array([name.startswith('m') for name in tnos['p']])]
#     classicals_aei(classicals)

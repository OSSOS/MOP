
__author__ = 'Michele Bannister   git:@mtbannister'

import sys
import numpy

import matplotlib.pyplot as plt
#import matplotlib.lines as mlines
from astropy import units

import palettable
from ossos import (parsers, parameters)
from src.ossos.planning.plotting import plot_fanciness


def full_aei(data_release, fov, icut=False, aiq=False):
    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.15)

    ymin = -0.001
    orbit_classes = ['cen', 'cla', 'res', 'sca', 'det', 'jc', 'xxx']
    colmap = palettable.wesanderson.Zissou_5.mpl_colors
    col = [colmap[4]] + colmap[0:4] + ['m'] + ['k']

    if parameters.RELEASE_VERSION == '4':
        # don't want uncharacterised for this plot
        data = data_release[numpy.array([name.startswith("o") for name in data_release['object']])]
        imax = 55
        emax = 0.45 #.65
        qmin = data['peri'].min() - 4
        qmax = data['peri'].max() + 4
        xinner = 25
        xouter = 90
        annotation = False
        e45_annotation = False

    if parameters.RELEASE_VERSION == '8':
        imax = 35 #55
        emax = 0.49 #.99
        xinner = 36 #10
        xouter = 56 #720
        xstep = 2 #50
        ms = 5
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([nobs > 7 for nobs in data_release['nobs']])]
        # data = data_release[numpy.array([name.startswith("o4h") for name in data_release['object']])]
        plot_resonances = True

    if parameters.RELEASE_VERSION == '11' and fov == 'medium':
        imax = 55
        emax = 0.86
        xinner = 5
        xouter = 105
        xstep = 5
        ms = 4
        cold_alpha = 0.85  # when plotting blue only  # 0.7
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        # data = data[numpy.array([n < 4 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if parameters.RELEASE_VERSION == '11' and fov == 'classical':
        imax = 45
        emax = 0.53  #0.37
        xinner = 38
        xouter = 55
        xstep = 5
        ms = 3
        cold_alpha = 0.65
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        # data = data[numpy.array([n < .15 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        # data = data[numpy.array([n < .02 for n in data['e_e']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if parameters.RELEASE_VERSION == '11' and fov == 'thousand_au':
        imax = 70
        emax = 0.99
        xinner = 0
        xouter = 850
        xstep = 50
        ms = 6
        cold_alpha = 0.85
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        plot_resonances = True

    if parameters.RELEASE_VERSION == '10' and fov == 'medium':
        imax = 55
        emax = 0.86
        xinner = 5
        xouter = 105
        xstep = 5
        ms = 4
        cold_alpha = 0.85  # when plotting blue only  # 0.7
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        # data = data[numpy.array([n < 4 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if parameters.RELEASE_VERSION == '9' and fov == 'medium':
        imax = 47
        emax = 0.8
        xinner = 10
        xouter = 85
        xstep = 5
        ms = 6
        cold_alpha = 0.65  # when plotting blue only  # 0.7
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        # data = data[numpy.array([n < 4 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if parameters.RELEASE_VERSION == '9' and fov == 'classical':
        imax = 45
        emax = 0.53  #0.37
        xinner = 30 #38
        xouter = 60 #49
        xstep = 5
        ms = 5
        cold_alpha = 0.65
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        data = data[numpy.array([n < .15 for n in data['e_a']])]  # couple of big-uncertainty objects making things messy
        data = data[numpy.array([n < .02 for n in data['e_e']])]  # couple of big-uncertainty objects making things messy
        plot_resonances = True

    if parameters.RELEASE_VERSION == '9' and fov == 'thousand_au':
        imax = 70
        emax = 0.99
        xinner = 0
        xouter = 850
        xstep = 50
        ms = 6
        cold_alpha = 0.85
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([name.__contains__("nt") is False for name in data_release['object']])]
        plot_resonances = True

    data['peri'] = data['a'] * (1. - data['e'])
    data['e_peri'] = (data['e_a'] / data['a']) + (data['e_e'] / data['e'])

    data.sort('cl')

    coldcol = col
    hotcol = col[2]
    e45col = col #[palettable.wesanderson.Moonrise5_6.mpl_colors[4]]
    # cold_alpha = 0.85  # when plotting blue only  # 0.7
    hot_alpha = 0.25
    grid_alpha = 0.2
    ebarcolor = 'k'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'

    # sans_o3e45 = data[numpy.where(data['object'] != 'o3e45')]
    # if icut:
    #     cold = sans_o3e45[numpy.where(sans_o3e45['i'] < 5.)]
    #     hot = sans_o3e45[numpy.where(sans_o3e45['i'] >= 5.)]
    # else:  # no splits whatsoever
    #     cold = sans_o3e45
    cold = data

    # want to show o3e45 with a different symbol.
    # o3e45 = data[numpy.where(data['object'] == 'o3e45')]

    ax = helio_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes, annotation=annotation)
    if icut:
        ax = helio_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, orbit_classes, annotation=annotation)
    # ax = helio_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, ['cla'], annotation=e45_annotation)
    ax[0].set_ylim([ymin, imax])

    ax = a_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes, annotation=annotation)
    if icut:
        ax = a_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, annotation=annotation)
    # ax = a_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, ['cla'], annotation=e45_annotation)
    ax[1].set_ylim([ymin, imax])

    if aiq:
        ax = a_q(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, '$i < 5^{\circ}$',
                 1, annotation=annotation)
        if icut:
            ax = a_q(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, annotation=annotation)
        ax = a_q(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, annotation=e45_annotation)
        ax[2].set_ylim([qmin, qmax])
    else:
        if not icut:
            label = '$i < 5^{\circ}$'
        ax = a_e(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, label,
                 1, orbit_classes, annotation=annotation)
        if icut:
            ax = a_e(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, orbit_classes, annotation=annotation)
        # ax = a_e(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, ['cla'], annotation=e45_annotation)
        ax[2].set_ylim([ymin, emax])

    plt.xlim([xinner, xouter])
    ax[0].set_xticks(list(range(xinner, xouter, xstep)))
    ax[1].set_xticks(list(range(xinner, xouter, xstep)))
    ax[2].set_xticks(list(range(xinner, xouter, xstep)))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])

    if plot_resonances:
        resonances(ax, imax, emax)

    handles, labels = ax[2].get_legend_handles_labels()
    # handles.append(mlines.Line2D([], [], marker='*', color=col[0], alpha=cold_alpha, linestyle=None))
    labels = ['centaurs', 'classical', 'resonant', 'scattering', 'detached', 'unclassified', 'Jupiter-coupled']#, 'o3e45']
    #print len(handles)
    #print len(labels)
    #ax[0].legend(handles, labels, loc='upper right', numpoints=1, fontsize='small')

    plt.xlabel('semimajor axis (AU)')

    plt.draw()
    if aiq:
        type = 'aiq'
    else:
        type = 'aei'
    outfile = 'OSSOS_v{}_discoveries_{}_{}-{}_ilabel.pdf'.format(parameters.RELEASE_VERSION, type, xinner, xouter)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')
    sys.stdout.write('{}\n'.format(outfile))


def helio_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        print(i, orbclass)
        tnos = data[numpy.where(data['cl'] == orbclass)]

        ax[0].errorbar(tnos['dist'], tnos['i'],
                       # xerr=tnos['e_dist'], yerr=tnos['e_i'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i],
                       mec=colour[i], mew=0.5)

    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)
    ax[0].set_xlabel('heliocentric distance (AU)')

    if annotation:
        for obj in data:
            ax[0].annotate(obj['object'], (obj['dist'] + 0.7, obj['i']), size=5)

    return ax


def a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        ax[1].errorbar(tnos['a'], tnos['i'],
                       # xerr=tnos['e_a'], yerr=tnos['e_i'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5)
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[1].annotate(obj['object'], (obj['a'] + 0.7, obj['i']), size=5)

    return ax


def a_e(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        label = orbclass
        ax[2].errorbar(tnos['a'], tnos['e'],
                       # xerr=tnos['e_a'], yerr=tnos['e_e'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5,
                       label=label)
    ax[2].set_ylabel('eccentricity')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['e']), size=5)

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
    strong_ids = ['1:1', '2:1', '3:1', '4:1', '9:1', '5:1', #'6:1', '7:1', '8:1', '9:1', '10:1', '11:1',
               '3:2', '5:2', '7:2', '9:2', #'11:2',
               ]
    weak_ids = [
               '4:3', '5:3', '8:3', # '7:3', '11:3',
               '5:4', '7:4',
               '9:5',
               '10:7',
               #'11:6',
               #'12:5', #'12:7',
               '13:5', #'13:7',
               '15:8',
               #'16:9', # '16:3',
               '17:7', # '17:9',
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
            ax[1].axvline(a_res, alpha=0.15, zorder=0)
            ax[1].annotate(res, (a_res, imax-i_lower[k]), rotation=90, ha='center', size=textsize[k], alpha=0.7)
            ax[2].axvline(a_res, alpha=0.15, zorder=0)
            ax[2].annotate(res, (a_res, emax-e_lower[k]), rotation=90, ha='center', size=textsize[k], alpha=0.7)
    a_U = 19.18916464
    ax[1].axvline(a_U, alpha=0.15, zorder=0)
    ax[1].annotate('Uranus 1:1', (a_U, imax), rotation=90, ha='center', size='xx-small', alpha=0.7)
    ax[2].axvline(a_U, alpha=0.15, zorder=0)
    ax[2].annotate('Uranus 1:1', (a_U, emax), rotation=90, ha='center', size='xx-small', alpha=0.7)

    return


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
    ax.set_xticks(list(range(5, 40, 10)))
    ax.set_ylim([34., 48.])
    ax.set_yticks(list(range(36, 50, 4)))
    ax.set_ylabel('perihelion (AU)')
    plot_fanciness.remove_border(ax)

    plt.draw()
    outfile = 'OSSOS_v{}_classical_iq.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')



def classicals_aeiq(data):
    # designed to match the parameters and look of Petit et al. 2011
    # layout: top left, a-q, top right, i-q, lower left, a-i, lower right, a-e.
    data['peri'] = data['a'] * (1. - data['e'])
    data['e_peri'] = (data['e_a'] / data['a']) + (data['e_e'] / data['e'])

    # o3e45 = data[numpy.where(data['object'] == 'o3e45')]
    classicals = data[numpy.where(data['p'] == 'm')]
    outer = data[numpy.where(data['p'] == 'o')]
    outer = outer[numpy.where(outer['i'] < 5.)]
    print(outer)

    ms = 7
    capsize = 2  # error bar cap width
    fmt = ['.', '*']
    colour = ['b', 'r']
    alpha = 0.4

    fig, ax = plt.subplots(2, 2, figsize=(10, 10))
    fig.subplots_adjust(hspace=0.07)  # want to work on the v-space as well

    for i, data in enumerate([classicals, outer]):
    # top left: semimajor axis vs perihelion
        ax[0][0].errorbar(data['a'], data['peri'],
                          xerr=data['e_a'],
                          # no yerr?
                          fmt=fmt[i], alpha=alpha, ecolor='0.1', capsize=capsize, ms=ms, mfc=colour[i])
        ax[0][0].grid(True, alpha=0.4)
        # semimajor axis
        ax[0][0].set_xlim([38., 49.])
        # pericentre
        ax[0][0].set_ylabel('pericenter (AU)')
        ax[0][0].set_ylim([34., 47.])
        ax[0][0].set_yticks(list(range(34, 47, 2)))

        # top right: inclination vs perihelion
        ax[0][1].errorbar(data['i'], data['peri'],
                          xerr=data['e_i'],
                          fmt=fmt[i], alpha=alpha, ecolor='0.1', capsize=capsize, ms=ms, mfc=colour[i])
        ax[0][1].grid(True, alpha=0.4)
        ax[0][1].set_xlabel('inclination (deg)')
        ax[0][1].set_xlim([0., 35.])
        ax[0][1].set_xticks(list(range(0, 35, 5)))
        # pericentre
        ax[0][1].set_ylim([34., 47.])
        ax[0][1].set_yticks(list(range(34, 47, 2)))

        # lower left: semimajor axis vs inclination
        ax[1][0].errorbar(data['a'], data['i'],
                          xerr=data['e_a'], yerr=data['e_i'],
                          fmt=fmt[i], alpha=alpha, ecolor='0.1', ms=ms, capsize=capsize, mfc=colour[i])
        ax[1][0].set_xlabel('semimajor axis (AU)')
        ax[1][0].set_xlim([38., 49.])
        ax[1][0].set_xticks(list(range(38, 49, 2)))
        ax[1][0].set_ylabel('inclination (deg)')
        ax[1][0].set_ylim([0., 35.])
        ax[1][0].set_yticks(list(range(0, 35, 5)))
        ax[1][0].grid(True, which='both', alpha=0.4)

        # lower right: semimajor axis vs eccentricity
        ax[1][1].errorbar(data['a'], data['e'],
                          xerr=data['e_a'], yerr=data['e_e'],
                          fmt=fmt[i], alpha=alpha, ecolor='0.1', ms=ms, capsize=capsize, mfc=colour[i])
        ax[1][1].grid(True, alpha=0.4)
        ax[1][1].set_ylabel('eccentricity')
        ax[1][1].set_ylim([0., 0.25])
        ax[1][1].set_xlabel('semimajor axis (AU)')
        ax[1][1].set_xlim([38., 49.])
        ax[1][1].set_xticks(list(range(38, 49, 2)))

    plot_fanciness.remove_border(ax[0][0])
    # ax[0][0].tick_params(labelbottom='off')
    plot_fanciness.remove_border(ax[0][1])
    plot_fanciness.remove_border(ax[1][0])
    plot_fanciness.remove_border(ax[1][1], remove=['right', 'top'], keep=[])
    # ax[1][1].tick_params(labelbottom='off', labelleft='off')

    plt.draw()
    outfile = 'OSSOS_v{}_classical_aeiq.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


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
    tnos = parsers.ossos_release_parser(table=True)  # return as an astropy.Table.Table

    mag = tnos[numpy.where(tnos['mag'] < 23.5)]
    ossos = []
    for t in mag:
        if t['object'].startswith('o'):
            ossos.append(t)
    print(len(ossos))

    sys.exit()

    # argperi_a(parameters.REAL_KBO_AST_DIR)

    # fov = ['medium', 'classical', 'thousand_au']
    # full_aei(tnos, fov[0])
    # classicals_qi(tnos)

    classicals_aeiq(tnos)

__author__ = 'Michele Bannister and Jean-Marc Petit'

import os

import matplotlib.pyplot as plt
from astropy.table import Table
import numpy as np

from palettable.colorbrewer import sequential
from palettable import tableau
import prettyplotlib as ppl
import plot_fanciness
from utils import read_smooth_fit, square_fit

path = '/Users/bannisterm/Dropbox/Papers in progress/OSSOS/First_quarter/data/'

def plot_smooth_fit(i, block, ax, colours, pwd, offset=0, single=False):
    characterisation = {'13AE': 24.09, '13AO': 24.40}
    ls = ['-', '--', ':']

    smooth_parameter_files = filter(lambda name: name.startswith('smooth'), os.listdir('{}/{}/'.format(pwd, block)))
    smooth_parameter_files.sort(key=lambda x: float(x.split('-')[1]))
    x = np.arange(20.5, 25.1, 0.01)
    if single:  # just do the first one, the motion rate targeted by the main survey objectives
        smooth_parameter_files = smooth_parameter_files[0:1]
    for j, fn in enumerate(smooth_parameter_files):
        pd, ps, mag_limit = read_smooth_fit('{}/{}/{}'.format(pwd, block, fn))
        ys = square_fit(x, ps)

        print block, fn, mag_limit, square_fit(mag_limit, ps)

        if single:  # only want to add label to the legend if there is only one given
            ax[i].plot(x + (offset * j), ys * 100.,
                       ls=ls[j], color=colours[j],
                       label='smooth fit {}-{} "/hr overall'.format(fn.split('-')[1], fn.split('-')[2]))
        else:
            ax[i].plot(x + (offset * j), ys * 100.,
                       ls=ls[j], color=colours[j])

        # last, add the vertical line for characterisation limit for the block
        if j == len(smooth_parameter_files) - 1:
            # defined on target KBO population, modulo tracking efficiency & other factors: not on 40% mag limit
            ax[i].vlines(characterisation[block], 0., 90,
                         linestyles=':', alpha=0.7)
            ax[i].annotate("$m_{{characterized}}$ = {:.2f}".format(characterisation[block]),
                           (characterisation[block], 90), size=10, color='k', ha='center')


def plot_eff_data(i, block, ax, colours, pwd, offset):
    fmt = ['^', 'x', 'o']
    filenames = filter(lambda name: name.endswith('mag-rate.eff'), os.listdir('{}/{}/'.format(pwd, block)))
    filenames.sort(key=lambda x: float(x.split('-')[0]))
    for j, fn in enumerate(filenames):
        fn_pwd = '{}/{}/{}'.format(pwd, block, fn)
        eff = Table.read(fn_pwd, format='ascii', guess=False, delimiter=' ', data_start=0, comment='#',
                         names=['mag', 'eff', 'num_planted', 'd_eff_plus', 'd_eff_minus'],
                         header_start=None)
        ax[i].errorbar(eff['mag'] + (offset * j), eff['eff'] * 100., yerr=eff['d_eff_plus'] * 100.,
                       fmt=fmt[j], capsize=1, elinewidth=0.5, ms=3.5,
                       label='{} "/hr'.format(fn.split('_')[0]),
                       mfc=colours[j], mec=colours[j], ecolor=colours[j])  # enforce: colour_cycle didn't set all


def plot_eff_by_rate_of_motion(ax, blocks):
    pwd = path + 'efficiency_motion_rates'
    outfile = 'OSSOS_e-o_efficiency.pdf'
    colours = sequential.Greens_6.mpl_colors[::-1]  # want efficiency for most-targeted <8"/hr objects most prominent
    colours = colours[0:2] + [colours[3]]
    offset = 0.017  # set the error bars of data off from each other slightly for legibility; also displace smooth fits

    for i, block in enumerate(blocks):
        plot_smooth_fit(i, block, ax, colours, pwd, offset=offset)
        plot_eff_data(i, block, ax, colours, pwd, offset)
        ax[i].grid(True, alpha=0.3)
        ax[i].set_ylabel('efficiency (%)')
        plot_fanciness.remove_border(ax[i])
        ppl.legend(ax[i], loc='lower left', title=block, numpoints=1, fontsize='small', handletextpad=0.5)

    return outfile


def plot_eff_by_user(ax, blocks):
    pwd = path + 'efficiency_personal_bias/'
    outfile = 'e-o_efficiency_by_user.pdf'
    col = tableau.ColorBlind_10.mpl_colors
    col = col[0:5] + [col[8]]

    # no. files examined, from plots Charles made: see http://wiki.ossos-survey.org/index.php/Core_Teleconf_2014_10_21
    examined = {'13AE': {'jjk': 163, 'jkavelaars': 109, 'mtb55': 418},
                '13AO': {'jkavelaars': 53, 'mtb55': 71, 'bgladman': 111, 'montys': 440, 'ptsws': 74}}
    # swap out user ID for anonymous 'pnum' values in the final displayed plot
    user_blindness = {'mtb55': 'p1',
                      'jjk': 'p2',
                      'jkavelaars': 'p3',
                      'montys': 'p4',
                      'bgladman': 'p5',
                      'ptsws': 'p6'
                      }
    # Line colour and symbol consistent between plots for participant.
    colours = {'jjk': col[2], 'jkavelaars': col[1], 'mtb55': col[0], 'bgladman': col[3], 'montys': col[4],
               'ptsws': col[5]}
    markers = {'jjk': '^', 'jkavelaars': 'd', 'mtb55': '*', 'bgladman': 'o', 'montys': '.', 'ptsws': 'x'}

    for i, block in enumerate(blocks):
        user_eff_files = filter(lambda x: x.__contains__(block), os.listdir(pwd))
        # user_eff_files.sort(key=lambda x: examined[block][x.split('.')[2]], reverse=True)  # biggest to smallest
        user_eff_files.sort(key=lambda x: user_blindness[x.split('.')[2]])
        for j, fn in enumerate(user_eff_files):
            eff = Table.read(pwd + fn, names=['mag', 'eff'], format='ascii')
            user = fn.split('.')[2]
            ax.plot(eff['mag'], eff['eff'] * 100,
                       c=colours[user], marker=markers[user], ms=5, mec=colours[user], alpha=0.7,
                       # line thickness scaled by number of files examined: most first so that thickest line at the back
                       linewidth=examined[block][user] * 0.02,
                    label="{}: {}".format(user_blindness[user], examined[block][user]),
                    zorder=1 - examined[block][user])
        # not the appropriate smooth fit to be adding here.
        # plot_smooth_fit(i, block, ax, ['k'], path + 'efficiency_motion_rates', single=True)

        ax.grid(True, alpha=0.3)
        ax.set_ylabel('efficiency (%)')
        ax.set_ylim([0., 100.])
        plot_fanciness.remove_border(ax)
        ppl.legend(ax, loc='lower left', title=block, numpoints=1, fontsize='small', handletextpad=0.5)

    return outfile


if __name__ == '__main__':
    fig, ax = plt.subplots(nrows=2, ncols=1, sharex=True, figsize=(9, 4))
    fig.subplots_adjust(hspace=0.1)
    blocks =  ['13AE', '13AO'] # [',['13AO']  #

    outfile = plot_eff_by_rate_of_motion(ax, blocks)
    # outfile = plot_eff_by_user(ax, blocks)

    plt.xlabel("$m_{r}$")
    plt.xlim([21.1, 25.])
    plt.draw()
    plt.savefig(outfile, transparent=True, bbox_inches='tight')

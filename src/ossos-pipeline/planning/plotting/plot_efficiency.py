__author__ = 'Michele Bannister and Jean-Marc Petit'

import os

import matplotlib.pyplot as plt
from astropy.table import Table
import numpy as np
import palettable.colorbrewer.sequential as palette
import prettyplotlib as ppl

import plot_fanciness


pwd = '/Users/michele/Dropbox/Papers in progress/OSSOS/First_quarter/data/efficiency_motion_rates'
characterisation = {'13AE': 24.04, '13AO': 24.39}
blocks = ['13AE', '13AO']
outfile = 'OSSOS_e-o_efficiency.pdf'


def read_smooth_fit(fichier):
    with open(fichier, 'r') as infile:
        lines = infile.readlines()
        for line in lines:
            if line[0] != "#":
                if line.startswith("double_param="):
                    pd = map(float, line[13:-1].split())
                if line.startswith("square_param="):
                    ps = map(float, line[13:-1].split())
                if line.startswith("mag_lim="):
                    mag_limit = float(line.split()[1])

    return pd, ps, mag_limit


def square_fit(m, params):
    return np.where(m < 21., params[0],
                    (params[0] - params[1] * (m - 21.0) ** 2) / (1. + np.exp((m - params[2]) / params[3])))


def plot_smooth_fit(i, block, ax):
    smooth_parameter_files = filter(lambda name: name.startswith('smooth'), os.listdir('{}/{}/'.format(pwd, block)))
    smooth_parameter_files.sort(key=lambda x: float(x.split('-')[1]))
    x = np.arange(21., 25.1, 0.01)
    for j, fn in enumerate(smooth_parameter_files):
        pd, ps, mag_limit = read_smooth_fit('{}/{}/{}'.format(pwd, block, fn))
        ys = square_fit(x, ps)
        ax[i].plot(x, ys,
                   ls='-', color=colours[j])

        # last, add the vertical line for characterisation limit for the block
        if j == 2:  # defined on target KBO population, modulo tracking efficiency & other factors: not 40% mag limit
            ax[i].vlines(characterisation[block], 0., 0.4,
                         linestyles=':', alpha=0.7)
            ax[i].annotate("$m_{{characterized}}$ = {:.2f}".format(characterisation[block]),
                           (characterisation[block], 0.4), size=7, color='k')


def plot_eff_data(i, block, ax):
    filenames = filter(lambda name: name.endswith('mag-rate.eff'), os.listdir('{}/{}/'.format(pwd, block)))
    filenames.sort(key=lambda x: float(x.split('-')[0]))
    for j, fn in enumerate(filenames):
        fn_pwd = '{}/{}/{}'.format(pwd, block, fn)
        eff = Table.read(fn_pwd, format='ascii', guess=False, delimiter=' ', data_start=0, comment='#',
                         names=['mag', 'eff', 'num_planted', 'd_eff_plus', 'd_eff_minus'],
                         header_start=None)
        ax[i].errorbar(eff['mag'] + (offset * j), eff['eff'], yerr=eff['d_eff_plus'],
                       fmt=fmt[j], alpha=alpha, capsize=capsize, elinewidth=0.5, ms=ms,
                       label='{} "/hr'.format(fn.split('_')[0]),
                       mfc=colours[j], mec=colours[j], ecolor=colours[j])  # enforce: colour_cycle didn't set all


alpha = 1
grid_alpha = 0.3
colours = palette.Greens_6.mpl_colors[::-1]  # want efficiency for most-targeted <8"/hr objects to be most prominent
colours = colours[0:2] + [colours[3]]
capsize = 1  # error bar cap width
fmt = ['*', 'x', 'o']
linestyle = ['-', '-.', ':']
ms = 3.5

fig, ax = plt.subplots(nrows=2, ncols=1, sharex=True)
fig.subplots_adjust(hspace=0.05)
offset = 0.017  # set the error bars of data off from each other slightly for legibility

for i, block in enumerate(blocks):
    plot_smooth_fit(i, block, ax)
    plot_eff_data(i, block, ax)
    ax[i].grid(True, alpha=grid_alpha)
    ax[i].set_ylabel('efficiency')
    plot_fanciness.remove_border(ax[i])
    ppl.legend(ax[i], loc='lower left', title=block, numpoints=1, fontsize='small', handletextpad=0.5)

plt.xlabel("$r'_{AB}$")
plt.xlim([21., 25.])
plt.draw()
plt.savefig(outfile, transparent=True, bbox_inches='tight')


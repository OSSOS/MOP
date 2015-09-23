__author__ = 'Michele Bannister   git:@mtbannister'

import os

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
from astropy.table import Table

import prettyplotlib as ppl
import plot_fanciness
from parsers import ossos_discoveries
from parameters import RELEASE_VERSION
from ossos import mpc, orbfit

path = '/Users/bannisterm/Dropbox/Papers in progress/OSSOS/First_quarter/data/'


def cfeps_residuals(version='L7', regenerate=False):
    if version == 'L4':
        header = ['obsnum', 'arclen', 'date', 'ra_resid', 'date2', 'dec_resid']
        data = Table.read(path + 'L4resid.txt',
                          format='ascii', guess=False, delimiter=' ', data_start=0, names=header)
        cfeps_mas = (np.sqrt(data['ra_resid'] ** 2 + data['dec_resid'] ** 2)) * 1000

    else:
        all_resids = []
        if regenerate:
            data_path = '/Users/bannisterm/Dropbox/mpc/'
            for filename in os.listdir(data_path):
                observations = mpc.MPCReader(data_path + filename)
                print observations.provisional_name
                orbit = orbfit.Orbfit(observations.mpc_observations)
                try:
                    residuals = orbit.residuals(overall=True)
                    all_resids += residuals
                except Exception, e:
                    print observations.provisional_name
                    print e
                    continue
            print all_resids

            with open('cfeps_resids_{}.dat'.format(version), 'w') as outfile:
                for r in all_resids:
                    outfile.write("{}\n".format(r))

        else:
            assert os.path.exists('cfeps_resids_{}.dat'.format(version))
            with open('cfeps_resids_{}.dat'.format(version), 'r') as infile:
                lines = infile.readlines()
                for line in lines:
                    all_resids.append(float(line))
            print len(all_resids)

    cfeps_mas = np.array(all_resids)
    print cfeps_mas
    print 'cfeps', np.median(cfeps_mas)

    return cfeps_mas


def ossos_residuals(version=RELEASE_VERSION, regenerate=False):
    all_resids = []  # arcsec

    if regenerate:
        # takes a few minutes to compute all these
        discoveries = ossos_discoveries(data_release=version)  # gets the full MPC lines for the current release
        for obj in discoveries:
            print obj.name
            residuals = obj.orbit.residuals(overall=True)
            all_resids += residuals
        print all_resids

        with open('ossos_resids_v{}.dat'.format(RELEASE_VERSION), 'w') as outfile:
            for r in all_resids:
                outfile.write("{}\n".format(r))

    else:
        assert os.path.exists('ossos_resids_v{}.dat'.format(RELEASE_VERSION))
        with open('ossos_resids_v{}.dat'.format(RELEASE_VERSION), 'r') as infile:
            lines = infile.readlines()
            for line in lines:
                all_resids.append(float(line))
        print len(all_resids)

    ossos_mas = np.array(all_resids)
    print ossos_mas
    print 'ossos', np.median(ossos_mas)

    return ossos_mas


if __name__ == '__main__':
    fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5, 5))

    cfeps_mas = cfeps_residuals(version='L7')
    ossos_mas = ossos_residuals()

    bins = 16
    maxrange = .85
    ax.hist(cfeps_mas,
            histtype='step', bins=bins, range=(0, maxrange),  # these seem appropriate
            color='r', linestyle='solid', lw=2.1, alpha=0.3, label='CFEPS L7')
    ax.hist(ossos_mas,
            histtype='step', bins=bins, range=(0, maxrange),
            color='b', linestyle='solid', label='OSSOS 13A')

    ax.grid(True, alpha=0.3)
    plt.xlabel("astrometric residual (arcsec)")
    plt.ylabel("number of astrometric measurements")
    plt.xlim([0, .85])

    handler_map = [
        mlines.Line2D([], [], color='r', linestyle='solid', lw=2.1, alpha=0.3, label='CFEPS L7'),
        mlines.Line2D([], [], color='b', label='OSSOS 13A'),
    ]

    ppl.legend(title="Survey",
               fontsize='small', handletextpad=0.5)
    plot_fanciness.remove_border(ax)

    plt.draw()
    outfile = path.rpartition('data')[0] + 'figures/astrometric_residual_50mas_bins.pdf'
    plt.savefig(outfile, transparent=True, bbox_inches='tight')

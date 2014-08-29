__author__ = 'Michele Bannister   git:@mtbannister'

import os

import astropy  # specifically written for v 0.2.4. Later versions have different coordinate.ra/de print methods.

from ossos import mpc
from ossos.orbfit import Orbfit


date = '2015-04-15T00:00:00'
lightcurve_targets = '/Users/michele/Dropbox/Telescope proposals/Subaru proposal/lightcurve_targets.txt'
ossinpath = '/Users/michele/Dropbox/OSSOS/measure3/ossin/'
outfile = '/Users/michele/Dropbox/Telescope proposals/Subaru proposal/lc_pos_20150414.txt'

with open(outfile, 'w') as ofile:
    ofile.write('Target	RA (hrs)	DEC		m_r	delta RA (")	delta DEC (")	Time predicted\n')

with open(lightcurve_targets, 'r') as infile:
    lines = infile.readlines()
    for line in lines:
        obj, mag = line.split('\t')

        fname = ossinpath + obj + '.ast'
        if not os.path.exists(fname):
            print obj
            fname = ossinpath + obj + 'PD.ast'

        mpc_observations = mpc.MPCReader(fname).mpc_observations
        orbit = Orbfit(mpc_observations)
        orbit.predict(date=date)

        with open(outfile, 'a') as ofile:
            ofile.write("{:>10s} {:>10s} {:>10s} {:>10s} {:6.2f} {:6.2f} {:>10s}".format(
                obj, orbit.coordinate.ra.format(unit=astropy.units.core.Unit("hour"), sep=':'),
                orbit.coordinate.dec.format(sep=':'),
                mag.strip('\n'), orbit.dra, orbit.ddec, date))

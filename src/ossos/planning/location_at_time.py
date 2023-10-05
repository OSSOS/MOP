__author__ = 'Michele Bannister   git:@mtbannister'

import os
import astropy  # specifically written for v 0.2.4. Later versions have different coordinate.ra/de print methods.
from astropy import units as u

from ossos import mpc
from ossos import storage
from ossos.orbfit import Orbfit

date = '2016-10-24T16:00:00'
# lightcurve_targets = '/Users/michele/Dropbox/Telescope proposals/Subaru ' \
#                      'proposal_2015A_lightcurves/lightcurve_targets.txt'
ossinpath = '/Users/bannisterm/Dropbox/OSSOS/measure3/ossin/tmp/' #'vos:OSSOS/dbaseclone/ast/'  #
outfile = '/Users/bannisterm/Desktop/{}.txt'.format(date)
# '/Users/michele/Dropbox/Telescope proposals/Subaru proposal/lc_pos_20150414.txt'

with open(outfile, 'w') as ofile:
    ofile.write('Target	RA (hrs)	DEC		m_r	delta RA (")	delta DEC (")	Time predicted\n')

# with open(lightcurve_targets, 'r') as infile:
#     lines = infile.readlines()
#     for line in lines:
#         obj, mag = line.split('\t')
for kbo_filename in os.listdir(ossinpath):
    mpc_observations = mpc.MPCReader(ossinpath + kbo_filename).mpc_observations
    orbit = Orbfit(mpc_observations)
    orbit.predict(date=date)

    with open(outfile, 'a') as ofile:
        ofile.write("{:>10s} {:>10s} {:6.2f} {:6.2f} {:>10s}\n".format(
            orbit.name, orbit.coordinate.to_string('hmsdms', sep=':'),
            # orbit.coordinate.dec.to_string('hmsdms'),  # need to add mag back in
            orbit.dra, orbit.ddec, date))

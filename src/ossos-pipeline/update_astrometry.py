import re
import math
import ephem

__author__ = 'jjk'
"""
Update the astrometry and photometry of an mpc observation based on the header contents of the observation.
 """


from ossos import mpc, storage, wcs
from ossos.gui import logger
import argparse


def remeasure(mpc_line):
    """
    re-measure the astrometry and photometry of the given mpc line
    """
    # TODO  Actually implement this.
    mpc_obs = mpc.Observation.from_string(mpc_line)
    if mpc_obs.null_observation:
        return
    assert isinstance(mpc_obs.comment, mpc.MPCComment)
    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)',mpc_obs.comment.frame)
    print parts.group('expnum'), parts.group('ccd')
    header = storage.get_astheader(parts.group('expnum'),int(parts.group('ccd')))
    this_wcs = wcs.WCS(header)
    (ra,dec) = this_wcs.xy2sky(float(mpc_obs.comment.X),float(mpc_obs.comment.Y))
    dr = math.sqrt((ra-mpc_obs.coordinate.ra.degrees)**2+(dec-mpc_obs.coordinate.dec.degrees)**2)*3600.0
    if dr > 0.5:
        print "{} ({:08.2f},{:08.2f}) {}".format(mpc_obs.comment.frame,float(mpc_obs.comment.X),
                                                   float(mpc_obs.comment.Y), dr)
        print mpc_obs.ra, ephem.hours(math.radians(ra))
        print mpc_obs.dec, ephem.degrees(math.radians(dec))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('mpc_file',help="An MPC file to update.")

    args = parser.parse_args()

    with open(args.mpc_file) as f:
        for line in f:
            remeasure(line)


import re

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
    header = storage.get_astheader(parts.group('expnum'),int(parts.group('ccd')))
    this_wcs = wcs.WCS(header)
    (x,y) = this_wcs.sky2xy(mpc_obs.coordinate.ra.degrees, mpc_obs.coordinate.dec.degrees)
    mpc_obs.comment.X = x
    mpc_obs.comment.Y = y
    print mpc_obs.to_string()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('mpc_file',help="An MPC file to update.")

    args = parser.parse_args()

    fptr = storage.open_vos_or_local(args.mpc_file)
    lines = fptr.read().split('\n')
    for line in lines:
        if not len(line) > 0:
            continue
        remeasure(line)


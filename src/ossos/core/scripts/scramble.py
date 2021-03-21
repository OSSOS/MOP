#!python 
################################################################################
##                                                                            ##
## Copyright 2013 by its authors                                              ##
## See COPYING, AUTHORS                                                       ##
##                                                                            ##
## This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)              ##
##                                                                            ##
##    OSSOS-MOP is free software: you can redistribute it and/or modify       ##
##    it under the terms of the GNU General Public License as published by    ##
##    the Free Software Foundation, either version 3 of the License, or       ##
##    (at your option) any later version.                                     ##
##                                                                            ##
##    OSSOS-MOP is distributed in the hope that it will be useful,            ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of          ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ##
##    GNU General Public License for more details.                            ##
##                                                                            ##
##    You should have received a copy of the GNU General Public License       ##
##    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.      ##
##                                                                            ##
################################################################################
"""plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around."""


import argparse
import os
import sys
from ossos import storage
from ossos import util
from astropy.io import fits
import logging


def scramble(expnums, ccd, version='p', dry_run=False):
    """run the plant script on this combination of exposures"""

    mjds = []
    fobjs = []
    for expnum in expnums:
        filename = storage.get_image(expnum, ccd=ccd, version=version)
        fobjs.append(fits.open(filename))
        # Pull out values to replace in headers.. must pull them
        # as otherwise we get pointers...
        mjds.append(fobjs[-1][0].header['MJD-OBS'])

    order = [0, 2, 1]
    for idx in range(len(fobjs)):
        logging.info("Flipping %d to %d" % (fobjs[idx][0].header['EXPNUM'],
                                            expnums[order[idx]]))
        fobjs[idx][0].header['EXPNUM'] = expnums[order[idx]]
        fobjs[idx][0].header['MJD-OBS'] = mjds[order[idx]]
        uri = storage.get_uri(expnums[order[idx]],
                              ccd=ccd,
                              version='s',
                              ext='fits')
        fname = os.path.basename(uri)
        if os.access(fname, os.F_OK):
            os.unlink(fname)
        fobjs[idx].writeto(fname)
        if dry_run:
            continue
        storage.copy(fname, uri)

    return


def main():
    """Run the script."""

    parser = argparse.ArgumentParser()
    parser.add_argument('--ccd',
                        action='store',
                        type=int,
                        default=None,
                        help='which ccd to process')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnums",
                        type=int,
                        nargs=3,
                        help="expnums to scramble")
    parser.add_argument("--nosort",
                        action='store_true',
                        default=False,
                        help="do not sort before processing")
    parser.add_argument("--type",
                        action='store',
                        default='p',
                        choices=['p', 'o'],
                        help='which type of image')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", '-d',
                        action='store_true')
    parser.add_argument("--dry-run", action="store_true", help="Do not copy back to VOSpace, implies --force")
    parser.add_argument("--force", action='store_true')
    
    args = parser.parse_args()
    prefix = ""
    task = util.task()
    dependency = "update_header"

    storage.DBIMAGES = args.dbimages

    ## setup logging
    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO

    logging.basicConfig(level=level, format="%(message)s")
    logging.getLogger('vos').setLevel(level)
    logging.getLogger('vos').addHandler(logging.StreamHandler())
    
    if not args.nosort:
        args.expnums.sort()

    ccds = [args.ccd]
    exit_status = 0
    if args.ccd is None:
        ccds = list(range(0, 36))
    for ccd in ccds:
        storage.set_logger(task, prefix, args.expnums[0], ccd, args.type, args.dry_run)
        expnums = args.expnums
        if not (args.force or args.dry_run) and storage.get_status(task, prefix, expnums[0], version='s', ccd=ccd):
            continue
        message = storage.SUCCESS

        try:
            scramble(expnums=expnums, ccd=ccd, version='p', dry_run=args.dry_run)
        except Exception as e:
            logging.error(e)
            message = str(e)
            exit_status = message
        if not args.dry_run:
            storage.set_status(task, prefix, expnums[0], version='s', ccd=ccd, status=message)
    return exit_status


if __name__ == '__main__':
    sys.exit(main())

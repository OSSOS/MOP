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
import logging
import os
import sys

from astropy.io import fits

from ossos import storage
from ossos import util

dependency = "update_header"
task = 'scramble'


def scramble(expnums, ccd, version='p', dry_run=False, force=False, prefix=''):
    """
    run the plant script on this combination of exposures

    @param expnums: list of exposure numbers to scramble the time on
    @param ccd:  which CCD in (assumes this is a CFHT MegaCam MEF)
    @param version: should we scramble the 'p' or 'o' images?
    @param dry_run: if dry run then don't save back to VOSpace.
    @param force: if true then create scramble set, even if already exists.
    @param prefix: a string that will be pre-pended to the EXPNUM to get the filename, sometimes 'fk'.
    @return: None
    """

    # Get a list of the MJD values and then write a re-ordering of those into files with 's'
    # as their type instead of 'p' or 'o'
    mjds = []
    fobjs = []
    message = storage.SUCCESS
    if not (force or dry_run) and storage.get_status(task, prefix, expnums[0],
                                                     version='s', ccd=ccd):
        logging.info("{} recorded as complete for {} ccd {}".format(task,
                                                                    expnums, ccd))
        return

    with storage.LoggingManager(task, prefix, expnums[0], ccd, version):
        try:
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
                scramble_file_name = os.path.basename(uri)
                if os.access(scramble_file_name, os.F_OK):
                    os.unlink(scramble_file_name)
                fobjs[idx].writeto(scramble_file_name)
                if not dry_run:
                    storage.copy(scramble_file_name, uri)
            logging.info(message)
        except Exception as ex:
            message = str(ex)
            logging.error(message)

        if not dry_run:
            storage.set_status(task, prefix, expnum, version, ccd, status=message)

    return


def main():
    """Run the script."""

    parser = argparse.ArgumentParser(description='time scramble the input images by tweaking their headers.')
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

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()
    util.set_logger(args)
    logging.info('Starting {}'.format(cmd_line))

    storage.DBIMAGES = args.dbimages
    prefix = ''
    version = args.type

    if not args.nosort:
        args.expnums.sort()

    expnums = args.expnums
    if args.ccd is None:
        if int(expnums[0]) < 1785619:
            # Last exposures with 36 CCD Megaprime
            ccdlist = list(range(0, 36))
        else:
            # First exposrues with 40 CCD Megaprime
            ccdlist = list(range(0, 40))
    else:
        ccdlist = [args.ccd]
    for ccd in ccdlist:
        # check if scramble image alraedy made for this ccd
        scramble(expnums=expnums, ccd=ccd, version=version, dry_run=args.dry_run, prefix=prefix)


if __name__ == '__main__':
    sys.exit(main())

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
"""Run the OSSOS makepsf proceedure"""

import argparse
import os
from subprocess import CalledProcessError
import sys
import logging
from ossos import storage
from ossos import util


def mkpsf(expnum, ccd, version, dry_run=False, prefix=""):
    """Run the OSSOS jmpmakepsf script.

    """
    ## confirm destination directory exists.
    destdir = os.path.dirname(
        storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='fits'))
    if not dry_run:
        storage.mkdir(destdir)

    ## get image from the vospace storage area
    logging.info("Getting file from VOSpace")
    filename = storage.get_image(expnum, ccd, version=version, prefix=prefix)
    logging.info("Running mkpsf on %s %d" % (expnum, ccd))
    ## launch the makepsf script
    logging.info(util.exec_prog(['jmpmakepsf.csh',
                                 './',
                                 filename,
                                 'yes', 'yes']))

    if dry_run:
        return

    ## place the results into VOSpace
    basename = os.path.splitext(filename)[0]

    for ext in ('mopheader', 'psf.fits',
                'zeropoint.used', 'apcor', 'fwhm', 'phot'):
        dest = storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext=ext)
        source = basename + "." + str(ext)
        storage.copy(source, dest)

    return


def main(task='mkpsf'):

    parser = argparse.ArgumentParser(
        description='Run makepsf chunk of the OSSOS pipeline')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
                        default=None,
                        help='which ccd to process, default is all')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process")
    parser.add_argument("--dry-run",
                        action="store_true",
                        help="DRY RUN, don't copy results to VOSpace, implies --force")

    parser.add_argument("--fk", action="store_true", help="Run fk images")

    parser.add_argument("--type", "-t", choices=['o', 'p', 's'],
                        help="which type of image: o-RAW, p-ELIXIR, s-SCRAMBLE", default='p')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--force", default=False,
                        action="store_true")
    parser.add_argument("--debug", "-d",
                        action="store_true")

    args = parser.parse_args()

    util.set_logger(args)

    prefix = (args.fk and 'fk') or ''
    task  = util.task()
    dependency = 'mk_mopheader'

    storage.DBIMAGES = args.dbimages

    exit_code = 0
    for expnum in args.expnum:
        if args.ccd is None:
           if int(expnum) < 1785619:
               # Last exposures with 36 CCD Megaprime
               ccdlist = range(0,36)
           else:
               # First exposrues with 40 CCD Megaprime
               ccdlist = range(0, 40)
        else:
           ccdlist = [args.ccd]
        for ccd in ccdlist:
            if storage.get_status(task, prefix, expnum, version=args.type, ccd=ccd) and not args.force:
                logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, args.type, ccd))
                continue
            storage.set_logger(task,
                               prefix, expnum, ccd, args.type, args.dry_run)
            message = 'success'
            try:
                if not storage.get_status(dependency, prefix, expnum, "p", ccd=ccd):
                    raise IOError("{} not yet run for {}".format(dependency, expnum))
                mkpsf(expnum, ccd, args.type, args.dry_run, prefix=prefix)
                if args.dry_run:
                    continue
                storage.set_status('fwhm', prefix, expnum, version=args.type, ccd=ccd, status=str(storage.get_fwhm(
                    expnum, ccd, version=args.type)))
                storage.set_status('zeropoint', prefix, expnum, version=args.type, ccd=ccd,
                                   status=str(storage.get_zeropoint(
                                       expnum, ccd, version=args.type)))
            except Exception as e:
                message = str(e)

            logging.error(message)
            if not args.dry_run:
                storage.set_status(task, prefix, expnum, version=args.type, ccd=ccd, status=message)
    return exit_code

if __name__ == '__main__':
    sys.exit(main())

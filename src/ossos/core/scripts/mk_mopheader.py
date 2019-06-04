#!python 
# ###############################################################################
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
"""Run the OSSOS make mopheader proceedure"""

import argparse
import logging
import os
from subprocess import CalledProcessError
import sys

from ossos import storage
from ossos import util
from ossos import mopheader


def mk_mopheader(expnum, ccd, version, dry_run=False, prefix=""):
    """Run the OSSOS mopheader script.

    """
    ## confirm destination directory exists.
    destdir = os.path.dirname(
        storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='fits'))
    if not dry_run:
        storage.mkdir(destdir)

    ## get image from the vospace storage area
    filename = storage.get_image(expnum, ccd, version=version, prefix=prefix)
    logging.info("Running mopheader on %s %d" % (expnum, ccd))
    ## launch the mopheader script
    ## launch the makepsf script
    expname = os.path.basename(filename).strip('.fits')
    logging.info(util.exec_prog(['stepZjmp',
                                 '-f',
                                 expname]))

    mopheader_filename = expname+".mopheader"

    # mopheader_filename = mopheader.main(filename)

    if dry_run:
        return

    destination = storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='mopheader')
    source = mopheader_filename
    storage.copy(source, destination)

    return


def main():
    parser = argparse.ArgumentParser(
        description='Run mopheader chunk of the OSSOS pipeline')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
                        default=None,
                        help='which ccd to process, default is all')
    parser.add_argument('--ignore-update-headers', action='store_true', dest='ignore_update_headers')
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

    
    logging.info("started")
    prefix = (args.fk and 'fk') or ''
    task = util.task()
    dependency = 'update_header'

    storage.DBIMAGES = args.dbimages

    if args.ccd is None:
        ccdlist = list(range(0, 36))
    else:
        ccdlist = [args.ccd]

    exit_code = 0
    for expnum in args.expnum:
        for ccd in ccdlist:
            logging.info("Attempting to get status on header for {} {}".format(expnum, ccd))
            storage.set_logger(task,
                               prefix, expnum, ccd, args.type, args.dry_run)
            if storage.get_status(task, prefix, expnum, version=args.type, ccd=ccd) and not args.force:
                logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, args.type, ccd))
                continue
            message = 'success'
            try:
                logging.info("Building a header ")
                if not storage.get_status(dependency, prefix, expnum, "p", 36) and not args.ignore_update_headers:
                    raise IOError("{} not yet run for {}".format(dependency, expnum))
                mk_mopheader(expnum, ccd, args.type, args.dry_run, prefix=prefix)
            except CalledProcessError as cpe:
                message = str(cpe.output)
                exit_code = message
            except Exception as e:
                message = str(e)
            logging.info(message)

            logging.error(message)
            if not args.dry_run:
                storage.set_status(task, prefix, expnum, version=args.type, ccd=ccd, status=message)
    return exit_code


if __name__ == '__main__':
    sys.exit(main())

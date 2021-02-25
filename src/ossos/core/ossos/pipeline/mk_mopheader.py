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

from .. import storage
from .. import util
from .. import mopheader

task = "mk_mopheader"
dependency = 'update_header'


def run(expnum, ccd, version, dry_run=False, prefix="", force=False, ignore_dependency=False):
    """Run the OSSOS mopheader script.

    """
    message = storage.SUCCESS
    logging.info("Attempting to get status on header for {} {}".format(expnum, ccd))
    if storage.get_status(task, prefix, expnum, version, ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, version, ccd))
        return message

    with storage.LoggingManager(task, prefix, expnum, ccd, version, dry_run):
        try:
            logging.info("Building a mopheader ")
            if not storage.get_status(dependency, prefix, expnum, "p", 36) and not ignore_dependency:
                raise IOError("{} not yet run for {}".format(dependency, expnum))

            # confirm destination directory exists.
            destdir = os.path.dirname(storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='fits'))
            if not dry_run:
                storage.mkdir(destdir)

            # get image from the vospace storage area
            logging.info("Retrieving image from VOSpace")
            filename = storage.get_image(expnum, ccd, version=version, prefix=prefix)

            # launch the stepZjmp program         
            logging.info("Launching stepZ on %s %d" % (expnum, ccd))
            expname = os.path.basename(filename).split('.')[0]
            logging.info(util.exec_prog(['stepZjmp',
                                         '-f',
                                         expname]))
            # if this is a dry run then we are finished
            if dry_run:
                return message

            # push the header to the VOSpace
            mopheader_filename = expname+".mopheader"
            destination = storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='mopheader')
            source = mopheader_filename
            count = 0
            with open(source, 'r'):
              while True:
                try:
                    count += 1
                    logging.info("Attempt {} to copy {} -> {}".format(count, source, destination))
                    storage.copy(source, destination)
                    break
                except Exception as ex:
                    if count > 10:
                        raise ex
            logging.info(message)
        except CalledProcessError as cpe:
            message = str(cpe.output)
            logging.error(message)
        except Exception as e:
            message = str(e)
            logging.error(message)

        if not dry_run:
            storage.set_status(task, prefix, expnum, version=version, ccd=ccd, status=message)
        
    return message


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

    storage.DBIMAGES = args.dbimages

    if args.ccd is None:
        ccdlist = list(range(0, 36))
    else:
        ccdlist = [args.ccd]

    exit_code = 0
    for expnum in args.expnum:
        for ccd in ccdlist:
            run(expnum, ccd, args.type, args.dry_run, prefix=prefix, force=args.force, ignore_dependency=args.ignore_update_headers)


if __name__ == '__main__':
    main()

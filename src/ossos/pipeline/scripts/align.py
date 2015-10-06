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
from subprocess import CalledProcessError
import sys
from ossos import storage
from ossos import util
import logging


from ossos.pipeline import align


if __name__ == '__main__':
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
                        help="expnum(s) to process")
    parser.add_argument("--type",
                        action='store',
                        default='s',
                        choices=['s', 'p', 'o'],
                        help='which type of image')
    parser.add_argument('--no-sort',
                        action='store_true',
                        default=False,
                        help='do not sort exposure list by expnum before processing')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", '-d',
                        action='store_true')
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    if args.dry_run:
        args.force = True

    # # setup logging
    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO

    if not args.no_sort:
        args.expnums.sort()

    storage.DBIMAGES = args.dbimages
    prefix = ""
    task = util.task()
    dependency = "step1"

    logging.basicConfig(level=level, format="%(message)s")

    ccds = [args.ccd]
    if args.ccd is None:
        ccds = range(0, 36)

    for ccd in ccds:
        message = storage.SUCCESS
        try:
            storage.set_logger(task, prefix, args.expnums[0], ccd, args.type, args.dry_run)
            if not storage.get_status(dependency, prefix, args.expnums[0], version=args.type, ccd=ccd):
                raise IOError("mkpsf not yet run for %s ccd%s, can't run align" % (
                    str(args.expnums), str(ccd).zfill(2)))
            if storage.get_status(task, prefix, args.expnums[0], version=args.type, ccd=ccd) and not args.force:
                logging.info("align done for %s[%s]" % (args.expnums[0], ccd))
                continue
            align(args.expnums, ccd, version=args.type, dry_run=args.dry_run)
        except CalledProcessError as cpe:
            message = str(cpe)
            exit_code = message
	    logging.error("ERROR: {}".format(str(cpe)))
        except Exception as e:
	    logging.error("Exception: {}".format(str(e)))
            message = str(e)
        logging.critical(message)

        if not args.dry_run:
            storage.set_status(task, prefix, args.expnums[0], version=args.type, ccd=ccd, status=message)

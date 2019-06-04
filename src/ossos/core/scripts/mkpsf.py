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
from ossos.pipeline import mkpsf



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

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()

    prefix = (args.fk and 'fk') or ''
    storage.DBIMAGES = args.dbimages

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
        for ccd rrin ccdlist:
            mkpsf.run(expnum, ccd=ccd, version=version, dry_run=dry_run, prefix=prefix, force=force)

if __name__ == '__main__':
    sys.exit(main())

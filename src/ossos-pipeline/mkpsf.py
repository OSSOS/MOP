#/usr/bin/env python
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
import logging
import os
import os
from ossos import storage
from ossos import util 

def mkpsf(expnum, ccd, fversion):
    """Run the OSSOS makepsf script.

    """

    ## get image from the vospace storage area
    filename = storage.get_image(expnum, ccd, version=fversion)
    logging.info("Running mkpsf on %s %d" % (expnum, ccd))
    ## launch the makepsf script
    util.exec_prog(['jmpmakepsf.csh',
                          './',
                          filename,
                          'no'])

    ## place the results into VOSpace
    basename = os.path.splitext(filename)[0]

    ## confirm destination directory exists.
    destdir = os.path.dirname(
        storage.dbimages_uri(expnum, ccd, version=fversion,ext='fits'))
    logging.info("Checking that destination directories exist")
    storage.mkdir(destdir)


    for ext in ('mopheader', 'psf.fits',
                'zeropoint.used', 'apcor', 'fwhm', 'phot'):
        dest = storage.dbimages_uri(expnum, ccd, version=fversion, ext=ext)
        source = basename + "." + str(ext)
        logging.info("Copying %s -> %s" % ( source, dest))
        #storage.remove(dest)
        storage.copy(source, dest)

    return


if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description='Run makepsf chunk of the OSSOS pipeline')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
                        default=None,
                        help='which ccd to process, default is all'
                        )

    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode'
                        )

    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process"
                        )

    parser.add_argument("--type", "-t", choices=['o','p','s'], help="which type of image: o-RAW, p-ELIXIR, s-SCRAMBLE", default='p')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--force", default=False,
                        action="store_true")
    parser.add_argument("--debug", "-d",
                        action="store_true")

    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(message)s')
    if args.debug:
        logging.basicConfig(level=logging.DEBUG,         format="%(asctime)s - %(module)s.%(funcName)s %(lineno)d: %(message)s")


    storage.DBIMAGES = args.dbimages

    if args.ccd is None:
        ccdlist = range(0,36)
    else:
        ccdlist = [args.ccd]

    for expnum in args.expnum:
        for ccd in ccdlist:
            if storage.get_status(expnum, ccd, 'mkpsf', version=args.type) and not args.force:
                logging.info("Already did %s %s, skipping" %( str(expnum),
                                                                  str(ccd)))
                continue
            message = 'success'
            try:
                mkpsf(expnum, ccd, args.type)
                storage.set_status(expnum,
                                   ccd,
                                   'fwhm',
                                   version=args.type,
                                   status=str(storage.get_fwhm(
                    expnum, ccd, version=args.type)))
                storage.set_status(expnum,
                                   ccd,
                                   'zeropoint',
                                   version=args.type,
                                   status=str(storage.get_zeropoint(
                    expnum, ccd, version=args.type)))
                                         
            except Exception as e:
                message = str(e)

            logging.error(message)
            storage.set_status( expnum,
                                ccd,
                                'mkpsf',
                                version=args.type,
                                status=message)
                       

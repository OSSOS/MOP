#/usr/bin/env python
"""Run the OSSOS makepsf proceedure"""

_version = '1.0'

import argparse
import logging
import os
import ossos
from ossos import storage
from ossos import util 

def run_mkpsf(expnum, ccd):
    """Run the OSSOS makepsf script.

    """

    ## get image from the vospace storage area
    filename = ossos.storage.get_image(expnum, ccd, version='p')

    ## launch the makepsf script
    ossos.util.exec_prog(['jmpmakepsf.csh',
                          './',
                          filename,
                          'no'])

    ## place the results into VOSpace
    basename = os.path.splitext(filename)[0]

    for ext in ('mopheader', 'psf.fits',
                'zeropoint.used', 'apcor', 'fwhm'):
        dest = ossos.storage.dbimages_uri(expnum, ccd, version='p', ext=ext)
        source = basename + "." + ext
        ossos.storage.copy(source, dest)

    return


if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description='Run makepsf chunk of the OSSOS pipeline')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
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

    parser.add_argument("--version",
                        action='version',
                        version='%(prog)s '+_version 
                        )

    parser.add_argument("--verbose", "-v",
                        action="store_true")

    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(message)s')

    ossos.storage._dbimages = args.dbimages

    if not args.ccd:
        ccdlist = range(0,36)
    else:
        ccdlist = [args.ccd]

    for expnum in args.expnum:
        for ccd in ccdlist:
            logging.info("mkpsf on expnum %d ccd %d" % (expnum, ccd))
            run_mkpsf(expnum, ccd)


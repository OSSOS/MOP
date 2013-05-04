#!/usr/bin/env python
'''plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around.'''


import argparse
import os
from ossos import storage
from astropy.io import fits
import logging

def run_scramble(expnums, ccd, version='p'):
    '''run the plant script on this combination of exposures'''

    scramble = []
    headers = []
    fobjs = []
    for expnum in expnums:
        filename = storage.get_image(expnum, ccd=ccd, version=version)
        fobjs.append(fits.open(filename))
        headers.append(fobjs[-1][0].header)

    order = [1,0,2]
    for idx in range(len(fobjs)):
        fobjs[idx][0].header['EXPNUM'] = headers[order[idx]]['EXPNUM']
        fobjs[idx][0].header['MJD-OBS'] = headers[order[idx]]['MJD-OBS']
        uri = storage.get_uri(expnums[order[idx]],
                                ccd=ccd,
                                version='s',
                                ext='fits')
        fname = os.path.basename(uri)
        fobjs[idx].writeto(fname)
        storage.copy(fname, uri)

        # now make a link between files that the plant system will need
        for ext in ['apcor', 'obj.jmp', 'mopheader', 'phot',
                    'psf.fits','trans.jmp', 'zeropoint.used', 'fwhm']:
            storage.delete(expnums[order[idx]], ccd, 's', ext)
            storage.vlink(expnums[idx], ccd, 'p', ext,
                         expnums[order[idx]], ccd, 's', ext)

    return

if __name__=='__main__':
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
    parser.add_argument("--type",
                        action='store',
                        default='p',
                        choices=['p', 'o'],
                        help='which type of image')
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--debug",'-d',
                        action='store_true')
    args=parser.parse_args()

    ## setup logging
    level = logging.CRITICAL
    if args.debug:
        level = logging.DEBUG
    elif args.verbose:
        level = logging.INFO

    logging.basicConfig(level=level, format="%(message)s")

    ccds = [args.ccd]
    if args.ccd is None:
        ccds = range(0,36)
    
    for ccd in ccds:
        run_scramble(args.expnums,ccd=ccd, version=args.type)

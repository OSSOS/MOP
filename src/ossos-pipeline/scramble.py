#!/usr/bin/env python
'''plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around.'''


import argparse
import os
from ossos import storage
from astropy.io import fits
import logging

def scramble(expnums, ccd, version='p', dry_run=False):
    '''run the plant script on this combination of exposures'''

    mjds = []
    fobjs = []
    for expnum in expnums:
        filename = storage.get_image(expnum, ccd=ccd, version=version)
        fobjs.append(fits.open(filename))
        # Pull out values to replace in headers.. must pull them
        # as otherwise we get pointers...
        mjds.append(fobjs[-1][0].header['MJD-OBS'])

    order = [1,0,2]
    for idx in range(len(fobjs)):
        logging.info("Flipping %d to %d" % ( fobjs[idx][0].header['EXPNUM'],
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

        # now make a link ebetween files that the plant system will need
        for ext in ['apcor', 'obj.jmp', 'mopheader', 'phot',
                    'psf.fits','trans.jmp', 'zeropoint.used', 'fwhm']:
            if storage.exists(storage.get_uri(expnums[order[idx]], ccd, 's', ext)):
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
    parser.add_argument("--nosort",
                        action='store_true',
                        default=False,
                        help="do not sort before processing")
    parser.add_argument("--type",
                        action='store',
                        default='p',
                        choices=['p', 'o'],
                        help='which type of image')
    parser.add_argument("--verbose","-v",
                        action="store_true")
    parser.add_argument("--debug",'-d',
                        action='store_true')
    parser.add_argument("--dry_run", action="store_true", help="Do not copy back to VOSpace, implies --force")
    parser.add_argument("--force", action='store_true')
    
    args=parser.parse_args()

    if args.dry_run:
        args.force = True

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
    if args.ccd is None:
        ccds = range(0,36)
    for ccd in ccds:
        expnums = args.expnums
        if storage.get_status(expnums[0], ccd,
                              'scramble', version='s') and not args.force:
            continue
        message = storage.SUCCESS

        try:
            scramble(expnums=expnums, ccd=ccd, version='p', dry_run = args.dry_run)
        except Exception as e:
            logging.error(e)
            message = str(e)
        if not args.dry_run:
            storage.set_status(expnums[0], ccd,
                               'scramble', version='s',
                               status=message)
            

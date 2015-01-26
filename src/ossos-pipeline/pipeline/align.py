#!python 
"""plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around."""

import argparse
import json
from astropy import wcs
import os
from subprocess import CalledProcessError
import sys
from astropy.io import fits
from astropy.io import ascii
import math
import numpy
from ossos import storage
from ossos import util
import logging
from astropy.table import Table
from ossos import daophot


def get_wcs(shifts):
    # store the shifts as a WCS transform where the 'world' coordinates are really the pixel coordinates
    # in the reference frame.
    return wcs.WCS(header=shifts, naxis=2)


def align(expnums, ccd, version='s', dry_run=False):
    """Create a 'shifts' file that transforms the space/flux/time scale of all images to the first image.

    This function relies on the .fwhm, .trans.jmp, .phot and .zeropoint.used files for inputs.
    The scaling we are computing here is for use in planting sources into the image at the same sky/flux locations
    while accounting for motions of sources with time.

    :param expnums: list of MegaPrime exposure numbers to add artificial KBOs to,
                    the first frame in the list is the reference.
    :param ccd: which ccd to work on.
    :param version: Add sources to the 'o', 'p' or 's' images
    :param dry_run: don't push results to VOSpace.
    """

    # Get the images and supporting files that we need from the VOSpace area
    # get_image and get_file check if the image/file is already on disk.
    # re-computed fluxes from the PSF stars and then recompute x/y/flux scaling.

    # some dictionaries to hold the various scale
    pos = {}
    apcor = {}
    mags = {}
    zmag = {}
    mjdates = {}

    for expnum in expnums:
        filename = storage.get_image(expnum, ccd=ccd, version=version)
        zmag[expnum] = storage.get_zeropoint(expnum, ccd, prefix=None, version=version)
        mjdates[expnum] = float(fits.open(filename)[0].header.get('MJD-OBS'))
        apcor[expnum] = [float(x) for x in open(storage.get_file(expnum,
                                                                 ccd=ccd,
                                                                 version=version,
                                                                 ext=storage.APCOR_EXT)).read().split()]
        keys = ['crval1', 'cd1_1', 'cd1_2', 'crval2', 'cd2_1', 'cd2_2']
        # load the .trans.jmp values into a 'wcs' like dictionary.
        # .trans.jmp maps current frame to reference frame in pixel coordinates.
        # the reference frame of all the frames supplied must be the same.
        shifts = dict(zip(keys, [float(x) for x in open(storage.get_file(expnum,
                                                                         ccd=ccd,
                                                                         version=version,
                                                                         ext='trans.jmp')).read().split()]))
        shifts['crpix1'] = 0.0
        shifts['crpix2'] = 0.0
        # now create a wcs object based on those transforms, this wcs links the current frame's
        # pixel coordinates to the reference frame's pixel coordinates.
        w = get_wcs(shifts)

        # get the PHOT file that was produced by the mkpsf routine
        phot = ascii.read(storage.get_file(expnum, ccd=ccd, version=version, ext='phot'), format='daophot')

        # compute the small-aperture magnitudes of the stars used in the PSF
        mags[expnum] = daophot.phot(filename,
                                    phot['XCENTER'],
                                    phot['YCENTER'],
                                    aperture=apcor[expnum][0],
                                    sky=apcor[expnum][1] + 1,
                                    swidth=apcor[expnum][0],
                                    zmag=zmag[expnum])

        # covert the x/y positions to positions in Frame 1 based on the trans.jmp values.
        (x, y) = w.wcs_pix2world(mags[expnum]["XCENTER"], mags[expnum]["YCENTER"], 1)
        pos[expnum] = numpy.transpose([x, y])
        # match this exposures PSF stars position against those in the first image of the set.
        idx1, idx2 = util.match_lists(pos[expnums[0]], pos[expnum])

        # compute the magnitdue offset between the current frame and the reference.
        dmags = numpy.ma.array(mags[expnums[0]]["MAG"] - apcor[expnums[0]][2] -
                               (mags[expnum]["MAG"][idx1] - apcor[expnum][2]),
                               mask=idx1.mask)
        dmags.sort()

        # compute the median and determine if that shift is small compared to the scatter.
        dmag = dmags[int(len(dmags)/2.0)]
        if math.fabs(dmag) > 3*(dmags.std() + 0.01):
            logging.warn("Magnitude shift {} between {} and {} is large: {}".format(dmag,
                                                                                    expnums[0],
                                                                                    expnum,
                                                                                    shifts[expnum]))
        shifts['dmag'] = dmag
        shifts['emag'] = dmags.std()
        shifts['nmag'] = len(dmags.mask) - dmags.mask.sum()
        shifts['dmjd'] = mjdates[expnums[0]] - mjdates[expnum]
        shift_file = os.path.basename(storage.get_uri(expnum, ccd, version, '.shifts'))
        fh = open(shift_file, 'w')
        fh.write(json.dumps(shifts, sort_keys=True,indent=4, separators=(',', ': ')))
        fh.write('\n')
        fh.close()
        if not dry_run:
            storage.copy(shift_file, os.path.basename(storage.get_uri(expnum,ccd,version,'.shifts')))



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

    logging.basicConfig(level=level, format="%(message)s")

    ccds = [args.ccd]
    if args.ccd is None:
        ccds = range(0, 36)

    for ccd in ccds:
        message = storage.SUCCESS
        try:
            storage.set_logger(os.path.splitext(os.path.basename(sys.argv[0]))[0],
                               "", args.expnums[0], ccd, args.type, args.dry_run)
            if not storage.get_status(args.expnums[0], ccd,
                                      'step2', version=args.type):
                raise IOError("scramble not yet run for %s ccd%s" % (
                    str(args.expnums), str(ccd).zfill(2)))
            if storage.get_status(args.expnums[0], ccd,
                                  'align', version=args.type) and not args.force:
                logging.info("align done for %s[%s]" % (args.expnums[0], ccd))
                continue
            align(args.expnums, ccd, version=args.type, dry_run=args.dry_run)
        except CalledProcessError as cpe:
            message = str(cpe)
            exit_code = message
        except Exception as e:
            message = str(e)
        logging.critical(message)

        if not args.dry_run:
            storage.set_status(args.expnums[0],
                               ccd,
                               'align',
                               version=args.type,
                               status=message)

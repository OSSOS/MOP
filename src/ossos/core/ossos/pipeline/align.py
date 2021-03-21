"""Compute transformation needed to align images to common x/y grid."""
#
#  Copyright 2013 by its authors
#  See COPYING, AUTHORS
#
#  This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)
#
#    OSSOS-MOP is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    OSSOS-MOP is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.
#
import argparse
import json
import logging
import math
import os
import sys

import numpy
from astropy import wcs
from astropy.io import ascii
from astropy.io import fits
from ossos import daophot
from ossos import storage
from ossos import util

task = 'align'
dependency = 'step2'


class NpEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, numpy.integer):
            return int(obj)
        elif isinstance(obj, numpy.floating):
            return float(obj)
        elif isinstance(obj, numpy.ndarray):
            return obj.tolist()
        else:
            return super(NpEncoder, self).default(obj)


def get_wcs(shifts):
    # store the shifts as a WCS transform where the 'world' coordinates are really the pixel coordinates
    # in the reference frame.
    return wcs.WCS(header=shifts, naxis=2)


def align(expnums, ccd, version='s', prefix='', dry_run=False, force=True):
    """Create a 'shifts' file that transforms the space/flux/time scale of all
    images to the first image.

    This function relies on the .fwhm, .trans.jmp, .phot and .zeropoint.used files for inputs.
    The scaling we are computing here is for use in planting sources into the image at the same sky/flux locations
    while accounting for motions of sources with time.

    :param expnums: list of MegaPrime exposure numbers to add artificial KBOs to,
                    the first frame in the list is the reference.
    :param ccd: which ccd to work on.
    :param prefix: put this string in front of expnum when looking for exposure, normally '' or 'fk'
    :param force: When true run task even if this task is recorded as having succeeded
    :param version: Add sources to the 'o', 'p' or 's' images
    :param dry_run: don't push results to VOSpace.
    """
    message = storage.SUCCESS

    if storage.get_status(task, prefix, expnums[0], version, ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task,
                                                                        prefix,
                                                                        expnums[0],
                                                                        version,
                                                                        ccd))
        return

    # Get the images and supporting files that we need from the VOSpace area
    # get_image and get_file check if the image/file is already on disk.
    # re-computed fluxes from the PSF stars and then recompute x/y/flux scaling.

    # some dictionaries to hold the various scale
    pos = {}
    apcor = {}
    mags = {}
    zmag = {}
    mjdates = {}

    with storage.LoggingManager(task, prefix, expnums[0], ccd, version, dry_run):
        try:
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
                shifts = dict(list(zip(keys, [float(x) for x in
                                              open(storage.get_file(expnum,
                                                                    ccd=ccd,
                                                                    version=version,
                                                                    ext='trans.jmp')).read().split()])))
                shifts['crpix1'] = 0.0
                shifts['crpix2'] = 0.0
                # now create a wcs object based on those transforms, this wcs links the current frame's
                # pixel coordinates to the reference frame's pixel coordinates.
                w = get_wcs(shifts)

                # get the PHOT file that was produced by the mkpsf routine
                logging.debug("Reading .phot file {}".format(expnum))
                phot = ascii.read(storage.get_file(expnum, ccd=ccd, version=version, ext='phot'), format='daophot')

                # compute the small-aperture magnitudes of the stars used in the PSF
                logging.debug("Running phot on {}".format(filename))
                mags[expnum] = daophot.phot(filename,
                                            phot['XCENTER'],
                                            phot['YCENTER'],
                                            aperture=apcor[expnum][0],
                                            sky=apcor[expnum][1] + 1,
                                            swidth=apcor[expnum][0],
                                            zmag=zmag[expnum])

                # covert the x/y positions to positions in Frame 1 based on the trans.jmp values.
                logging.debug("Doing the XY translation to refrence frame: {}".format(w))
                (x, y) = w.wcs_pix2world(mags[expnum]["XCENTER"], mags[expnum]["YCENTER"], 1)
                pos[expnum] = numpy.transpose([x, y])
                # match this exposures PSF stars position against those in the first image of the set.
                logging.debug("Matching lists")
                idx1, idx2 = util.match_lists(pos[expnums[0]], pos[expnum])

                # compute the magnitdue offset between the current frame and the reference.
                dmags = numpy.ma.array(mags[expnums[0]]["MAG"] - apcor[expnums[0]][2] -
                                       (mags[expnum]["MAG"][idx1] - apcor[expnum][2]),
                                       mask=idx1.mask)
                dmags.sort()
                # logging.debug("Computed dmags between input and reference: {}".format(dmags))
                error_count = 0

                error_count += 1
                logging.debug("{}".format(error_count))

                # compute the median and determine if that shift is small compared to the scatter.
                try:
                    midx = int(numpy.sum(numpy.any([~dmags.mask], axis=0)) / 2.0)
                    dmag = float(dmags[midx])
                    logging.debug("Computed a mag delta of: {}".format(dmag))
                except Exception as e:
                    logging.error(str(e))
                    logging.error("Failed to compute mag offset between plant and found using: {}".format(dmags))
                    dmag = 99.99

                error_count += 1
                logging.debug("{}".format(error_count))

                try:
                    if math.fabs(dmag) > 3 * (dmags.std() + 0.01):
                        logging.warning("Magnitude shift {} between {} and {} is large: {}".format(dmag,
                                                                                                   expnums[0],
                                                                                                   expnum,
                                                                                                   shifts))
                except Exception as e:
                    logging.error(str(e))

                error_count += 1
                logging.debug("{}".format(error_count))

                shifts['dmag'] = dmag
                shifts['emag'] = dmags.std()
                shifts['nmag'] = len(dmags.mask) - dmags.mask.sum()
                shifts['dmjd'] = mjdates[expnums[0]] - mjdates[expnum]
                shift_file = os.path.basename(storage.get_uri(expnum, ccd, version, '.shifts'))

                error_count += 1
                logging.debug("{}".format(error_count))

                try:
                    fh = open(shift_file, 'w')
                    fh.write(json.dumps(shifts, sort_keys=True, indent=4, separators=(',', ': '),
                                        cls=NpEncoder))
                    fh.write('\n')
                    fh.close()
                except Exception as e:
                    logging.error("Creation of SHIFTS file failed while trying to write: {}".format(shifts))
                    raise e

                error_count += 1
                logging.debug("{}".format(error_count))

                if not dry_run:
                    storage.copy(shift_file, storage.get_uri(expnum, ccd, version, '.shifts'))
            logging.info(message)
        except Exception as ex:
            message = str(ex)
            logging.error(message)

        if not dry_run:
            storage.set_status(task, prefix, expnum, version, ccd, status=message)


def main():
    """
    Run the align process.
    @return: None
    """

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
    parser.add_argument("--fk", help="add the fk prefix on processing?",
                        default=False,
                        action='store_true')
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

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()
    util.set_logger(args)
    logging.info("Starting {}".format(cmd_line))

    storage.DBIMAGES = args.dbimages

    prefix = (args.fk and 'fk') or ''
    version = args.type

    if not args.no_sort:
        args.expnums.sort()

    if args.ccd is None:
        if int(args.expnums[0]) < 1785619:
            # Last exposures with 36 CCD Megaprime
            ccdlist = list(range(0, 36))
        else:
            # First exposrues with 40 CCD Megaprime
            ccdlist = list(range(0, 40))
    else:
        ccdlist = [args.ccd]

    for ccd in ccdlist:
        align(args.expnums, ccd, version=version, prefix=prefix,
              dry_run=args.dry_run)


if __name__ == '__main__':
    main()

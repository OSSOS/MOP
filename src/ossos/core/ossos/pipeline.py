import json
import logging
import os
import math

from astropy import wcs
from astropy.io import ascii
from astropy.io import fits
import numpy

import storage
import util


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
        logging.debug("Reading .phot file {}".format(expnum))
        phot = ascii.read(storage.get_file(expnum, ccd=ccd, version=version, ext='phot'), format='daophot')

        # compute the small-aperture magnitudes of the stars used in the PSF
        import daophot
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
        logging.debug("Computed dmags between input and reference: {}".format(dmags))
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
            fh.write(json.dumps(shifts, sort_keys=True, indent=4, separators=(',', ': ')))
            fh.write('\n')
            fh.close()
        except Exception as e:
            logging.error("Creation of SHIFTS file failed while trying to write: {}".format(shifts))
            raise e

        error_count += 1
        logging.debug("{}".format(error_count))

        if not dry_run:
            storage.copy(shift_file, storage.get_uri(expnum, ccd, version, '.shifts'))

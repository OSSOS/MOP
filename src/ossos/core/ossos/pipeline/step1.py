###############################################################################
#                                                                            ##
# Copyright 2013 by its authors                                              ##
# See COPYING, AUTHORS                                                       ##
#                                                                            ##
# This file is part of OSSOS Moving Object Pipeline (OSSOS-MOP)              ##
#                                                                            ##
#    OSSOS-MOP is free software: you can redistribute it and/or modify       ##
#    it under the terms of the GNU General Public License as published by    ##
#    the Free Software Foundation, either version 3 of the License, or       ##
#    (at your option) any later version.                                     ##
#                                                                            ##
#    OSSOS-MOP is distributed in the hope that it will be useful,            ##
#    but WITHOUT ANY WARRANTY; without even the implied warranty of          ##
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ##
#    GNU General Public License for more details.                            ##
#                                                                            ##
#    You should have received a copy of the GNU General Public License       ##
#    along with OSSOS-MOP.  If not, see <http://www.gnu.org/licenses/>.      ##
#                                                                            ##
###############################################################################
"""step1 is to run the two source finding algorithms in the image.

step1jmp is a stand-alone fortran code from Jean-Marc Petit et al.
step1matt is a script from M. Holman that runs E. Bertain's sExtractor.

"""

import argparse
from astropy.io import fits
import logging
import os
from ossos import storage
from ossos import util
import sys

_SEX_THRESHOLD = 1.1
_WAVE_THRESHOLD = 2.7
_FWHM = 4.0
_MAX_COUNT = 30000

dependency = "mkpsf"
task = "step1"


def _get_weight_map(filename, ccd):
    # for step1matt we need the weight image
    hdulist = fits.open(filename)
    flat_name = hdulist[0].header.get('FLAT', 'weight.fits')
    parts = os.path.splitext(flat_name)
    if parts[1] == '.fz':
        flat_name = os.path.splitext(parts[0])[0]
    else:
        flat_name = parts[0]

    logging.info("Getting the Flat Field image.")
    try:
        flat_filename = storage.get_image(flat_name, ccd, version='', ext='fits', subdir='calibrators')
    except Exception:
        flat_filename = storage.get_image(flat_name, ccd, version='', ext='fits', subdir='old_calibrators')

    if os.access('weight.fits', os.R_OK):
        os.unlink('weight.fits')

    if not os.access('weight.fits', os.R_OK):
        os.symlink(flat_filename, 'weight.fits')

    return flat_filename


def run(expnum,
        ccd,
        prefix='',
        version='p',
        sex_thresh=_SEX_THRESHOLD,
        wave_thresh=_WAVE_THRESHOLD,
        maxcount=_MAX_COUNT,
        dry_run=False,
        force=True):
    """run the actual step1jmp/matt codes.

    expnum: the CFHT expousre to process
    ccd: which ccd in the mosaic to process
    fwhm: the image quality, FWHM, of the image.  In pixels.
    sex_thresh: the detection threhold to run sExtractor at
    wave_thresh: the detection threshold for wavelet
    maxcount: saturation level

    """
    message = storage.SUCCESS

    if storage.get_status(task, prefix, expnum, version, ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, version, ccd))
        return

    with storage.LoggingManager(task, prefix, expnum, ccd, version, dry_run):
        try:
            if not storage.get_status(dependency, prefix, expnum, version, ccd):
                raise IOError(35, "Cannot start {} as {} not yet completed for {}{}{}{:02d}".format(
                    task, dependency, prefix, expnum, version, ccd))
            logging.info("Retrieving imaging and input parameters from VOSpace")
            storage.get_file(expnum, ccd, prefix=prefix, version=version, ext='mopheader')
            filename = storage.get_image(expnum, ccd, version=version, prefix=prefix)
            fwhm = storage.get_fwhm(expnum, ccd, prefix=prefix, version=version)
            basename = os.path.splitext(filename)[0]

            _get_weight_map(filename, ccd)

            logging.info("Launching step1jmp")
            logging.info(util.exec_prog(['step1jmp',
                                         '-f', basename,
                                         '-t', str(wave_thresh),
                                         '-w', str(fwhm),
                                         '-m', str(maxcount)]))

            logging.info(util.exec_prog(['step1matt',
                                         '-f', basename,
                                         '-t', str(sex_thresh),
                                         '-w', str(fwhm),
                                         '-m', str(maxcount)]))

            if os.access('weight.fits', os.R_OK):
                os.unlink('weight.fits')

            if not dry_run:
                for ext in ['obj.jmp', 'obj.matt']:
                    obj_uri = storage.get_uri(expnum, ccd, version=version, ext=ext,
                                              prefix=prefix)
                    obj_filename = basename + "." + ext
                    count = 0
                    with open(obj_filename, 'r'):
                        while True:
                            try:
                                count += 1
                                logging.info("Attempt {} to copy {} -> {}".format(count, obj_filename, obj_uri))
                                storage.copy(obj_filename, obj_uri)
                                break
                            except Exception as ex:
                                if count > 10:
                                    raise ex
            logging.info(message)
        except Exception as ex:
            message = str(ex)
            logging.error(message)

        if not dry_run:
            storage.set_status(task, prefix, expnum, version, ccd, status=message)


def main():
    # Must be running as a script

    parser = argparse.ArgumentParser(
        description='Run step1jmp and step1matt on a given exposure.')

    parser.add_argument("--ccd", "-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
    parser.add_argument("--ignore", help="Try to run even in previous step failed.",
                        default=False,
                        action="store_true")
    parser.add_argument("--fk", help="add the fk prefix on processing?",
                        default=False,
                        action='store_true')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("--sex_thresh",
                        action="store",
                        type=float,
                        default=_SEX_THRESHOLD,
                        help="sExtractor detection threhold")
    parser.add_argument("--wavelet_thresh",
                        type=float,
                        default=_WAVE_THRESHOLD,
                        help="Wavelet detection threhold")
    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process")
    parser.add_argument("--version",
                        action='version',
                        version='%(prog)s 1.0')
    parser.add_argument('--type', default='p',
                        choices=['o', 'p', 's'], help="which type of image")
    parser.add_argument('--log', default=None, help="Write standard out to this file")
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", '-d',
                        action='store_true')
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--dry-run", action="store_true", help="Do a dry run, not changes to vospce, implies --force")

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()

    util.set_logger(args)
    logging.info("Starting {}".format(cmd_line))

    storage.DBIMAGES = args.dbimages

    prefix = (args.fk and 'fk') or ''
    version = args.type

    for expnum in args.expnum:
        if args.ccd is None:
            if int(expnum) < 1785619:
                # Last exposures with 36 CCD Megaprime
                ccdlist = list(range(0, 36))
            else:
                # First exposrues with 40 CCD Megaprime
                ccdlist = list(range(0, 40))
        else:
            ccdlist = [args.ccd]
        for ccd in ccdlist:
            run(expnum,
                ccd,
                prefix=prefix,
                version=version,
                sex_thresh=args.sex_thresh,
                wave_thresh=args.wavelet_thresh,
                dry_run=args.dry_run,
                force=args.force)


if __name__ == '__main__':
    sys.exit(main())

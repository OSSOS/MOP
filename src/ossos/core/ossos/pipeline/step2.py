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
"""Run step2 of the OSSOS pipeline."""

import argparse
import errno
import logging
import math
import os
from astropy.io import fits
from ossos import storage
from ossos import util
from ossos import wcs
import sys

_FWHM = 4.0

task = util.task()
dependency = "step1"


def compute_trans(expnums, ccd, version, prefix=None, default="WCS"):
    """
    Pull the astrometric header for each image, compute an x/y transform and compare to trans.jmp

    this one overides trans.jmp if they are very different.
    @param expnums: three exposures to link together.
    @param ccd: which CCD of the mosaic to process
    @param version: which version of the exposure: o == raw, p == processed, s == scrambled
    @param prefix: string prefixed to the filenames (fk for planted images, blank otherwise)
    @param default: which transform to use ? WCS or JPM?
    @return: None
    """
    wcs_dict = {}
    for expnum in expnums:
        try:
            # TODO This assumes that the image is already N/E flipped.
            # If compute_trans is called after the image is retrieved from archive then we get the disk version.
            filename = storage.get_image(expnum, ccd, version, prefix=prefix)
            this_wcs = wcs.WCS(fits.open(filename)[0].header)
        except Exception as err:
            logging.warning("WCS Trans compute failed. {}".format(str(err)))
            return
        wcs_dict[expnum] = this_wcs
    x0 = wcs_dict[expnums[0]].header['NAXIS1'] / 2.0
    y0 = wcs_dict[expnums[0]].header['NAXIS2'] / 2.0
    (ra0, dec0) = wcs_dict[expnums[0]].xy2sky(x0, y0)
    result = ""
    for expnum in expnums:
        (x, y) = wcs_dict[expnum].sky2xy(ra0, dec0)
        dr = 1000
        try:
            filename = storage.get_file(expnum, ccd, version, ext='.trans.jmp', prefix=prefix)
            jmp_trans = open(filename, 'r').readline().split()
            x1 = float(jmp_trans[0]) + float(jmp_trans[1]) * x + float(jmp_trans[2]) * y
            y1 = float(jmp_trans[3]) + float(jmp_trans[4]) * x + float(jmp_trans[5]) * y
            dr = math.sqrt((x1 - x0) ** 2 + (y1 - y0) ** 2)
        except Exception:
            pass
        if dr > 0.5:
            result += "WARNING: WCS-JMP transforms mis-matched {} reverting to using {}.\n".format(expnum, default)
            if default == "WCS":
                uri = storage.dbimages_uri(expnum, ccd, version, ext='.trans.jmp', prefix=prefix)
                filename = os.path.basename(uri)
                trans = open(filename, 'w')
                trans.write("{:5.2f} 1. 0. {:5.2f} 0. 1.\n".format(x0 - x, y0 - y))
                trans.close()
        else:
            result += "WCS-JMP transforms match {}\n".format(expnum)
    return result


def run(expnums, ccd, version, prefix=None, dry_run=False, default="WCS", force=False):
    """run the actual step2  on the given exp/ccd combo"""

    jmp_trans = ['step2ajmp']
    jmp_args = ['step2bjmp']
    matt_args = ['step2matt_jmp']

    if storage.get_status(task, prefix, expnums[0], version, ccd) and not force:
        logging.info("{} completed successfully for {}{}{}{:02d}".format(
            task, prefix, expnums[0], version, ccd))
        return

    with storage.LoggingManager(task, prefix, expnums[0], ccd, version, dry_run):
        try:
            for expnum in expnums:
                if not storage.get_status(dependency, prefix, expnum, version=version, ccd=ccd):
                    raise IOError(35, "Cannot start {} as {} not yet completed for {}{}{}{:02d}".format(
                        task, dependency, prefix, expnum, version, ccd))
            message = storage.SUCCESS

            idx = 0
            logging.info("Retrieving catalog files to do matching.")
            for expnum in expnums:
                jmp_args.append(
                    storage.get_file(expnum, ccd=ccd, version=version, ext='obj.jmp', prefix=prefix)[0:-8]
                )
                jmp_trans.append(
                    storage.get_file(expnum, ccd=ccd, version=version, ext='obj.jmp', prefix=prefix)[0:-8]
                )
                idx += 1
                matt_args.append('-f%d' % idx)
                matt_args.append(
                    storage.get_file(expnum, ccd=ccd, version=version, ext='obj.matt', prefix=prefix)[0:-9]
                )

            logging.info("Computing the catalog alignment using sources in catalogs.")
            try:
                logging.info(util.exec_prog(jmp_trans))
                if default == "WCS":
                    logging.info("Comparing computed transform to WCS values")
                    logging.info(compute_trans(expnums, ccd, version, prefix, default=default))
            except Exception as ex:
                logging.info("JMP Trans failed: {}".format(ex))
                logging.info(compute_trans(expnums, ccd, version, prefix, default="WCS"))

            logging.info("Using transform to match catalogs for three images.")
            logging.info(util.exec_prog(jmp_args))
            logging.info(util.exec_prog(matt_args))

            # check that the shifts from step2 are rational by matching the bright star lists.
            logging.info("Uisng checktrans to ensure that transforms were reasonable.")
            check_args = ['checktrans']
            if os.access('proc-these-files', os.R_OK):
                os.unlink('proc-these-files')
            ptf = open('proc-these-files', 'w')
            ptf.write("# A dummy file that is created so checktrans could run.\n")
            ptf.write("# Frame FWHM PSF?\n")
            for expnum in expnums:
                filename = os.path.splitext(storage.get_image(expnum, ccd, version=version, prefix=prefix))[0]
                if not os.access(filename + ".bright.psf", os.R_OK):
                    os.link(filename + ".bright.jmp", filename + ".bright.psf")
                if not os.access(filename + ".obj.psf", os.R_OK):
                    os.link(filename + ".obj.jmp", filename + ".obj.psf")
                ptf.write("{:>19s}{:>10.1f}{:>5s}\n".format(filename,
                                                            _FWHM,
                                                            "NO"))
            ptf.close()
            if os.access('BAD_TRANS', os.F_OK):
                os.unlink('BAD_TRANS')

            logging.info(util.exec_prog(check_args))

            if os.access('BAD_TRANS', os.F_OK):
                raise OSError(errno.EBADMSG, 'BAD_TRANS')

            if os.access('proc-these-files', os.F_OK):
                os.unlink('proc-these-files')

            if dry_run:
                return

            for expnum in expnums:
                for ext in ['unid.jmp', 'unid.matt', 'trans.jmp']:
                    uri = storage.dbimages_uri(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)
                    filename = os.path.basename(uri)
                    storage.copy(filename, uri)

        except Exception as ex:
            message = str(ex)
            logging.error(message)

        storage.set_status(task, prefix, expnums[0], version, ccd, status=message)

    return


def main():
    # Must be running as a script

    parser = argparse.ArgumentParser(
        description='Run step2jmp and step2matt on a given exposure.')

    parser.add_argument("--ccd", "-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
    parser.add_argument("--fk", action="store_true", default=False, help="Do fakes?")
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnums",
                        type=int,
                        nargs=3,
                        help="3 expnums to process")
    parser.add_argument("--version",
                        action='version',
                        version='%(prog)s 1.0')
    parser.add_argument('-t', '--type',
                        help='which type of image to process',
                        choices=['s', 'p', 'o'],
                        default='p')
    parser.add_argument('--no-sort',
                        help='preserve input exposure order',
                        action='store_true')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--default", default="WCS",
                        choices=['WCS', 'JMP'],
                        help="Which shift should be used if they dis-agree?")
    parser.add_argument("--debug",
                        action="store_true")
    parser.add_argument("--dry-run", action="store_true", help="run without pushing back to VOSpace, implies --force")
    parser.add_argument("--force", action="store_true")

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()

    util.set_logger(args)
    logging.info("Starting {}".format(cmd_line))
    storage.DBIMAGES = args.dbimages
    prefix = (args.fk and "fk") or ""
    expnums = args.expnums
    version = args.type

    if args.ccd is None:
        expnum = min(expnums)
        if int(expnum) < 1785619:
            # Last exposures with 36 CCD Megaprime
            ccdlist = list(range(0, 36))
        else:
            # First exposrues with 40 CCD Megaprime
            ccdlist = list(range(0, 40))
    else:
        ccdlist = [args.ccd]

    if not args.no_sort:
        args.expnums.sort()

    for ccd in ccdlist:
        run(args.expnums, ccd, version=version, prefix=prefix, dry_run=args.dry_run, default=args.default, force=args.force)


if __name__ == '__main__':
    sys.exit(main())

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
"""plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around."""

import argparse
import json
from astropy import wcs
import os
from subprocess import CalledProcessError
import sys
import tempfile
from astropy.io import fits
import math
import errno
from ossos import storage
from ossos import util
import logging
from ossos.plant import KBOGenerator
from pyraf import iraf
from numpy import radians, fabs, log10, rint, cos, sin, transpose


def plant_kbos(filename, psf, kbos, shifts, prefix):
    """
    Add KBOs to an image
    :param filename: name of the image to add KBOs to
    :param psf: the Point Spread Function in IRAF/DAOPHOT format to be used by ADDSTAR
    :param kbos: list of KBOs to add, has format as returned by KBOGenerator
    :param shifts: dictionary with shifts to transfer to coordinates to reference frame.
    :param prefix: an estimate FWHM of the image, used to determine trailing.
    :return: None
    """

    iraf.set(uparm="./")
    iraf.digiphot()
    iraf.apphot()
    iraf.daophot(_doprint=0)
    iraf.images()

    if shifts['nmag'] < 4:
        logging.warning("Mag shift based on fewer than 4 common stars.")
        fd = open("plant.WARNING", 'a')
        fd.write("Mag shift based on fewer than 4 common stars.")
        fd.close()

    if shifts['emag'] > 0.05:
        logging.warning("Mag shift has large uncertainty.")
        fd = open("plant.WARNING", 'a')
        fd.write("Mag shift hsa large uncertainty.")
        fd.close()

    addstar = tempfile.NamedTemporaryFile(suffix=".add")

    # transform KBO locations to this frame using the shifts provided.
    w = get_wcs(shifts)

    header = fits.open(filename)[0].header

    # set the rate of motion in units of pixels/hour instead of ''/hour
    scale = header['PIXSCAL1']
    rate = kbos['sky_rate']/scale

    # compute the location of the KBOs in the current frame.

    # offset magnitudes from the reference frame to the current one.
    mag = kbos['mag'] - shifts['dmag']
    angle = radians(kbos['angle'])

    # Move the x/y locations to account for the sky motion of the source.
    x = kbos['x'] - rate*24.0*shifts['dmjd']*cos(angle)
    y = kbos['y'] - rate*24.0*shifts['dmjd']*sin(angle)
    x, y = w.wcs_world2pix(x, y, 1)

    # Each source will be added as a series of PSFs so that a new PSF is added for each pixel the source moves.
    itime = float(header['EXPTIME'])/3600.0
    npsf = fabs(rint(rate * itime)) + 1
    mag += 2.5*log10(npsf)
    dt_per_psf = itime/npsf

    # Build an addstar file to be used in the planting of source.
    idx = 0
    for record in transpose([x, y, mag, npsf, dt_per_psf, rate, angle]):
        x = record[0]
        y = record[1]
        mag = record[2]
        npsf = record[3]
        dt = record[4]
        rate = record[5]
        angle = record[6]
        for i in range(int(npsf)):
            idx += 1
            x += dt*rate*math.cos(angle)
            y += dt*rate*math.sin(angle)
            addstar.write("{} {} {} {}\n".format(x, y, mag, idx))

    addstar.flush()
    fk_image = prefix+filename
    try:
        os.unlink(fk_image)
    except OSError as err:
        if err.errno == errno.ENOENT:
            pass
        else:
            raise

    # add the sources to the image.
    iraf.daophot.addstar(filename, addstar.name, psf, fk_image,
                         simple=True, verify=False, verbose=False)
    # convert the image to short integers.
    iraf.images.chpix(fk_image, fk_image, 'ushort')


def get_wcs(shifts):
    # store the shifts as a WCS transform where the 'world' coordinates are really the pixel coordinates
    # in the reference frame.
    return wcs.WCS(header=shifts, naxis=2)


def plant(expnums, ccd, rmin, rmax, ang, width, number=10, mmin=21.0, mmax=25.5, version='s', dry_run=False):
    """Plant artificial sources into the list of images provided.

    :param expnums: list of MegaPrime exposure numbers to add artificial KBOs to
    :param ccd: which ccd to work on.
    :param rmin: The minimum rate of motion to add sources at (''/hour)
    :param rmax: The maximum rate of motion to add sources at (''/hour)
    :param ang: The mean angle of motion to add sources
    :param width: The +/- range of angles of motion
    :param version: Add sources to the 'o', 'p' or 's' images
    :param dry_run: don't push results to VOSpace.
    """

    # Construct a list of artificial KBOs with positions in the image
    # and rates of motion within the bounds given by the caller.
    filename = storage.get_image(expnums[0],
                                 ccd=ccd,
                                 version=version)
    header = fits.open(filename)[0].header
    bounds = util.get_pixel_bounds_from_datasec_keyword(header.get('DATASEC', '[33:2080,1:4612]'))

    # generate a set of artificial KBOs to add to the image.
    kbos = KBOGenerator.get_kbos(n=number,
                                 rate=(rmin, rmax),
                                 angle=(ang - width, ang + width),
                                 mag=(mmin, mmax),
                                 x=(bounds[0][0], bounds[0][1]),
                                 y=(bounds[1][0], bounds[1][1]),
                                 filename='Object.planted')

    for expnum in expnums:
        filename = storage.get_image(expnum, ccd, version)
        psf = storage.get_file(expnum, ccd, version, ext='psf.fits')
        plant_kbos(filename, psf, kbos, get_shifts(expnum, ccd, version), "fk")

    if dry_run:
        return

    uri = storage.get_uri('Object', ext='planted', version='',
                          subdir=str(
                              expnums[0]) + "/ccd%s" % (str(ccd).zfill(2)))

    storage.copy('Object.planted', uri)
    for expnum in expnums:
        uri = storage.get_uri(expnum,
                              ccd=ccd,
                              version=version,
                              ext='fits', prefix='fk')
        filename = os.path.basename(uri)
        storage.copy(filename, uri)

    return


def get_shifts(expnum, ccd, version):
    return json.loads(open(storage.get_file(expnum, ccd, version, ext='shifts')).read())


def main(argv):
    """

    @param argv: an array of arguments to be parsed, normally sys.argv
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
    parser.add_argument('--no-sort',
                        action='store_true',
                        default=False,
                        help='do not sort exposure list by expnum before processing')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", '-d',
                        action='store_true')
    parser.add_argument("--number", "-n", type=int, help="Number of KBOs to plant into images.", default=10)
    parser.add_argument("--mmin", type=float, help="Minimum magnitude value to add source with.", default=21.0)
    parser.add_argument("--mmax", type=float, help="Maximum magnitude value to add source with.", default=25.5)
    parser.add_argument("--rmin", default=0.5,
                        type=float, help="minimum motion rate")
    parser.add_argument("--rmax", default=15,
                        type=float, help="maximum motion rate")
    parser.add_argument("--width", default=30,
                        type=float, help="angle opening")
    parser.add_argument("--ang", default=20,
                        type=float, help="angle of motion, 0 is West")
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)

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
    task = util.task()
    prefix = ""
    dependency = 'mkpsf'

    ccds = [args.ccd]
    if args.ccd is None:
        ccds = list(range(0, 36))

    exit_code = 0
    for ccd in ccds:
        message = storage.SUCCESS
        try:
            storage.set_logger(task, prefix, args.expnums[0], ccd, args.type, args.dry_run)
            if not storage.get_status(dependency, prefix, args.expnums[0], version='s', ccd=ccd):
                raise IOError("{} not yet run for {} {} {}".format(dependency, args.expnums, args.type, ccd))
            if storage.get_status(task, prefix, args.expnums[0], version=args.type, ccd=ccd) and not args.force:
                logging.info("{} previously completed successfully for {}{}{:02d}".format(task,
                                                                                          args.expnums[0],
                                                                                          args.type,
                                                                                          ccd))
                continue
            plant(args.expnums,
                  ccd,
                  args.rmin, args.rmax, args.ang, args.width,
                  number=args.number, mmin=args.mmin, mmax=args.mmax,
                  version=args.type,
                  dry_run=args.dry_run)
        except CalledProcessError as cpe:
            message = str(cpe)
            exit_code = message
        except Exception as e:
            message = str(e)
        logging.critical(message)

        if not args.dry_run:
            storage.set_status(task, prefix, args.expnums[0], version=args.type, ccd=ccd, status=message)
    return exit_code


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

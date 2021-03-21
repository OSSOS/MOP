#!/home/eashton/anaconda2/bin/python
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
"""run step3 of the OSSOS pipeline."""

import os
import argparse
import logging
import sys
from ossos import util
from ossos import storage

_RATE_MIN = 0.5
_RATE_MAX = 15.0
_ANGLE_CENTRE = 23.0
_ANGLE_WIDTH = 30.0


def step3(expnums, ccd, version, rate_min,
          rate_max, angle, width, field=None, prefix=None, dry_run=False,
          maximum_flux_ratio=3, minimum_area=5, minimum_median_flux=1000.0):
    """run the actual step3  on the given exp/ccd combo"""

    jmp_args = ['step3jmp']
    matt_args = ['step3jjk']

    idx = 0
    cmd_args = []
    for expnum in expnums:
        idx += 1
        for ext in ['unid.jmp', 'unid.matt']:
            storage.get_file(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)
        image = os.path.splitext(os.path.basename(storage.get_uri(expnum, ccd, version=version, prefix=prefix)))[0]
        cmd_args.append('-f%d' % idx)
        cmd_args.append(image)

    cmd_args.extend(['-rn', str(rate_min),
                     '-rx', str(rate_max),
                     '-a', str(angle),
                     '-w', str(width)])
    jmp_args.extend(cmd_args)

    # Add some extra arguemnents for the ISO search.
    cmd_args.extend(['-fr', str(maximum_flux_ratio),
                     '-ma', str(minimum_area),
                     '-mf', str(minimum_median_flux)])
    matt_args.extend(cmd_args)

    logging.info(util.exec_prog(jmp_args))
    logging.info(util.exec_prog(matt_args))

    if dry_run:
        return

    if field is None:
        field = str(expnums[0])
    storage.mkdir(os.path.dirname(storage.get_uri(field,
                                                  ccd=ccd,
                                                  version=version,
                                                  prefix=prefix)))

    for ext in ['moving.jmp', 'moving.matt']:
        uri = storage.get_uri(field,
                              ccd=ccd,
                              version=version,
                              ext=ext,
                              prefix=prefix)
        filename = '%s%d%s%s.%s' % (prefix, expnums[0],
                                    version,
                                    str(ccd).zfill(2),
                                    ext)
        storage.copy(filename, uri)

    return


def main():
    ### Must be running as a script

    parser = argparse.ArgumentParser(
        description='Run step3jjk and step3matt on a given triple.')

    parser.add_argument("--ccd", "-c",
                        action="store",
                        default=None,
                        type=int,
                        dest="ccd")
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
    parser.add_argument('--fk', help='Do fakes?', default=False, action='store_true')
    parser.add_argument('--field',
                        help='a string that identifies which field is being searched',
                        nargs='?')
    parser.add_argument('--no-sort',
                        help='preserve input exposure order',
                        action='store_true')
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--rate_min", default=_RATE_MIN,
                        help='minimum rate to accept',
                        type=float)
    parser.add_argument('--rate_max', default=_RATE_MAX,
                        help='maximum rate to accept',
                        type=float)
    parser.add_argument('--angle', default=_ANGLE_CENTRE,
                        help='angle of x/y motion, West is 0, North 90',
                        type=float)
    parser.add_argument('--width', default=_ANGLE_WIDTH,
                        help='opening angle of search cone',
                        type=float)
    parser.add_argument('--minimum-area', default=5, help='Minimum size of source allowed')
    parser.add_argument('--minimum-median-flux', default=1000, help='Maximum value of the median flux from MATT.')
    parser.add_argument('--maximum-flux-ratio', default=3, help="Maximum radio between minimum and maximum flux.")
    parser.add_argument("--dry-run", action="store_true", help="do not copy to VOSpace, implies --force")
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--ignore", action="store_true")

    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format="%(module)s: %(message)s")
    if args.debug:
        logging.basicConfig(level=logging.DEBUG, format="%(module)s.%(funcName)s.%(lineno)d %(message)s")

    storage.DBIMAGES = args.dbimages

    if args.ccd is None:
        ccdlist = list(range(0, 36))
    else:
        ccdlist = [args.ccd]

    if not args.no_sort:
        args.expnums.sort()

    prefix = (args.fk and 'fk') or ''

    exit_status = 0
    for ccd in ccdlist:
        # util.set_logger(os.path.splitext(os.path.basename(sys.argv[0]))[0], prefix,
        #                    args.expnums[0], ccd, args.type, args.dry_run)
        message = storage.SUCCESS
        try:
            # if not args.ignore and not storage.get_status(args.expnums[0], ccd, prefix + 'step2', version=args.type):
            #     raise IOError(35, "did step2 run on %s" % str(args.expnums))
            # sys.stderr.write("Checking status\n")
            # sys.stderr.write("stepI for expnum: {} ccd: {} status: {}\n".format(args.expnums[0], ccd, storage.get_status(args.expnums[0], ccd, prefix + 'step3', version=args.type)))
            # if storage.get_status(args.expnums[0], ccd, prefix + 'step3', version=args.type) and not args.force:
            #     logging.critical("Status: stepI.py already run on {} [{}{:02d}]".format(args.expnums, args.type, ccd))
            #     continue
            step3(args.expnums, ccd, version=args.type,
                  rate_min=args.rate_min,
                  rate_max=args.rate_max,
                  angle=args.angle,
                  width=args.width,
                  field=args.field,
                  prefix=prefix,
                  dry_run=args.dry_run,
                  minimum_area=args.minimum_area,
                  maximum_flux_ratio=args.maximum_flux_ratio,
                  minimum_median_flux=args.minimum_median_flux)
        except Exception as e:
            message = str(e)
            exit_status = message

        logging.error(message)
        # if not args.dry_run:
        #     storage.set_status(args.expnums[0],
        #                        ccd,
        #                        prefix + 'step3',
        #                        version=args.type,
        #                        status=message)

    return exit_status


if __name__ == '__main__':
    sys.exit(main())

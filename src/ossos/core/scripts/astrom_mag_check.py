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
"""
Compare the measured fluxes of planted sources against those returned for by digiphot.
"""

__author__ = 'jjk'

import sys
import os
from ossos import astrom
from ossos import storage
import argparse
import logging
from ossos import util
from ossos.match import match_planted

logger = logging.getLogger('vos')
logger.setLevel(logging.CRITICAL)
logger.addHandler(logging.StreamHandler())

BRIGHT_LIMIT = 23.0
OBJECT_PLANTED = "Object.planted"
MINIMUM_BRIGHT_DETECTIONS = 5
MINIMUM_BRIGHT_FRACTION = 0.5

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('field')
    parser.add_argument('ccd')
    parser.add_argument('--expnum', default=None, help="Which exposure is the lead for this astrom file?")
    parser.add_argument('--astrom-filename', default=None, help="Give the astrom file directly instead of looking-up "
                                                                "using the field/ccd naming scheme.")
    parser.add_argument('--reals', action='store_true', default=False)
    parser.add_argument('--type', choices=['o', 'p', 's'], help="Which type of image.", default='s')
    parser.add_argument('--measure3', default='vos:OSSOS/measure3/2013B-L_redo/')
    parser.add_argument('--dbimages', default=None)
    parser.add_argument('--dry-run', action='store_true', default=False)
    parser.add_argument('--force', action='store_true', default=False)

    parser.add_argument('--object-planted', default=OBJECT_PLANTED,
                        help="Name of file contains list of planted objects.")
    parser.add_argument('--bright-limit', default=BRIGHT_LIMIT,
                        help="Sources brighter than this limit {} are used to diagnose planting issues.".format(
                            BRIGHT_LIMIT))
    parser.add_argument('--minimum-bright-detections', default=MINIMUM_BRIGHT_DETECTIONS,
                        help="required number of detections with mag brighter than bright-limit.")
    parser.add_argument('--minimum-bright-fraction', default=MINIMUM_BRIGHT_FRACTION,
                        help="minimum fraction of objects above bright limit that should be found.")
    args = parser.parse_args()

    logging.basicConfig(level=logging.CRITICAL)

    prefix = 'fk'
    ext = args.reals and 'reals' or 'cands'
    task = util.task()

    storage.MEASURE3 = args.measure3

    if args.dbimages is not None:
        storage.DBIMAGES = args.dbimages
        astrom.DATASET_ROOT = args.dbimages

    astrom_uri = storage.get_cands_uri(args.field,
                                       ccd=args.ccd,
                                       version=args.type,
                                       prefix=prefix,
                                       ext="measure3.{}.astrom".format(ext))

    if args.astrom_filename is None:
        astrom_filename = os.path.basename(astrom_uri)
    else:
        astrom_filename = args.astrom_filename

    if not os.access(astrom_filename, os.F_OK):
        astrom_filename = os.path.dirname(astrom_uri) + "/" + astrom_filename

    # Load the list of astrometric observations that will be looked at.
    fk_candidate_observations = astrom.parse(astrom_filename)
    if args.expnum is None:
        expnum = fk_candidate_observations.observations[0].expnum
    else:
        expnum = args.expnum

    storage.set_logger(os.path.splitext(os.path.basename(sys.argv[0]))[0], prefix, expnum, "", ext, args.dry_run)
    match_filename = os.path.splitext(os.path.basename(astrom_filename))[0] + '.match'

    exit_status = 0
    status = storage.SUCCESS
    try:
        if (not storage.get_status(task, prefix, expnum=expnum, version='', ccd=args.ccd)) or args.force:
            logging.info(("Comparing planted and measured magnitudes "
                          "for sources in {} and {}\n".format(args.object_planted, astrom_filename)))
            message = match_planted(fk_candidate_observations,
                                    match_filename=match_filename,
                                    object_planted=args.object_planted,
                                    bright_limit=args.bright_limit,
                                    minimum_bright_detections=args.minimum_bright_detections,
                                    bright_fraction=args.minimum_bright_fraction)
            match_uri = storage.get_cands_uri(args.field,
                                              ccd=args.ccd,
                                              version=args.type,
                                              prefix=prefix,
                                              ext="measure3.{}.match".format(ext), block=args.field)
            if not args.dry_run:
                storage.copy(match_filename, match_uri)
                uri = os.path.dirname(astrom_uri)
                keys = [storage.tag_uri(os.path.basename(astrom_uri))]
                values = [message]
                storage.set_tags_on_uri(uri, keys, values)
    except Exception as err:
        sys.stderr.write(str(err))
        status = str(err)
        exit_status = err.message

    if not args.dry_run:
        storage.set_status(task, prefix, expnum, version='', ccd=args.ccd, status=status)

    return exit_status


if __name__ == '__main__':
    logger.critical("STARTING")
    code = main()
    logger.critical("FINISHED")
    sys.exit(code)
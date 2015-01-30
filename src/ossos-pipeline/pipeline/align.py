#!python 
"""plant synthetic moving objects into a set of observations.

prior to planting, the headers of the objects may be swapped around."""

import argparse
import os
from subprocess import CalledProcessError
import sys
from ossos import storage
import logging


from ossos.pipeline import align


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

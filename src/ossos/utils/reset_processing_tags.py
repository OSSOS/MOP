#!python
"""
Given an exposure number and optional ccd values clear the OSSOS processing tags associated with the expnum in dbimages.

This script is part of the process for resetting the pipeline.
"""

import logging
import argparse
import sys

from ossos import storage





# FIXME: currently this doesn't clear the snr_13 tag

PROGRAMS = {'CALIBRATION': (('', 'fk'), ('mkpsf', 'zeropoint', 'fwhm', 'snr_13'), ('p', 's', '')),
            'DETECT': (('', 'fk'), ('step1', ), ('p', 's')),
            'ALIGN': (('', 'fk'), ('step2', ), ('p', 's')),
            'MOVING': (('', 'fk'), ('step3', ), ('p', 's')),
            'REAL': (('',), ('mkpsf', 'step1', 'step2', 'step3', 'combine'), ('p',)),
            'SCRAMBLE': (('',), ('step1', 'step2', 'step3', 'combine'), ('s',)),
            'PLANT': (('',), ('scramble', 'plant', 'astrom_mag_check'), ('s',)),
            'FAKES': (('fk',), ('step1', 'step2', 'step3', 'combine', 'astrom_mag_check'), ('s',)),
            }
PREP = ((('',), ('update_header',), ('o', 'p')),)
ALL_CCDS = list(range(37))

logging.basicConfig(level=logging.INFO)


def clear_tags(expnum, ops_set, my_ccds, dry_run=True):
    """
    Clear the tags for the given expnum/ccd set.
    @param ops:
    @param my_expnum:
    @return:
    """

    status = None
    for ops in ops_set:
      for prefix in ops[0]:
        for task in ops[1]:
          for version in ops[2]:
            for ccd in my_ccds:
                if not dry_run:
                    storage.set_status(task, prefix, expnum, version, ccd, status)
                else:
                    sys.stdout.write("{} {} {} {} {} {}\n".format(task, prefix, expnum, version, ccd, status))


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('expnums', nargs='+')
    parser.add_argument('--dbimages', default='vos:OSSOS/dbimages')
    parser.add_argument('--dry-run', action='store_true')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--ccd', action='append')
    group.add_argument('--PREP', dest='PREP', action='store_const', const=PREP)
    parser.add_argument('--ALL', help="Clear all processing tags except preproc and update_header",
                        dest='programs', action='store_const',
                        const=[PROGRAMS[program] for program in PROGRAMS])

    for program in PROGRAMS:
        parser.add_argument('--{}'.format(program),
                            dest='programs',
                            help="Clear tags associated with {}".format(PROGRAMS[program][1]),
                            action='append_const', const=PROGRAMS[program])

    opt = parser.parse_args()
    ccds = opt.ccd is not None and opt.ccd or ALL_CCDS

    storage.DBIMAGES = opt.dbimages

    if opt.PREP is not None:
        opt.programs = opt.PREP

    if opt.programs is None or not len(opt.programs) > 0:
        parser.error("Must specify at least one program group to clear tags for.")

    ccds = opt.PREP is not None and [40] or ccds

    for expnum in opt.expnums:
        clear_tags(expnum, opt.programs, ccds, dry_run=opt.dry_run)

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
            'REAL': (('',), ('mkpsf', 'step1', 'step2', 'step3', 'combine'), ('p',)),
            'SCRAMBLE': (('',), ('step1', 'step2', 'step3', 'combine'), ('s',)),
            'PLANT': (('',), ('scramble', 'plant', 'astrom_mag_check'), ('s',)),
            'FAKES': (('fk',), ('step1', 'step2', 'step3', 'combine', 'astrom_mag_check'), ('s',)),
            }
PREP = ((('',), ('update_header',), ('o', 'p')),)
ALL_CCDS = range(37)

logging.basicConfig(level=logging.INFO)


def clear_tags(my_expnum, ops_set, my_ccds, dry_run=True):
    """
    Clear the tags for the given expnum/ccd set.
    @param ops:
    @param my_expnum:
    @return:
    """

    for ops in ops_set:
      for fake in ops[0]:
        for my_program in ops[1]:
            for version in ops[2]:
                props = {}
                for ccd in my_ccds:
                    # print my_expnum, fake, my_program, version, ccd
                    key = storage.get_process_tag(fake + my_program, ccd, version)
                    props[key] = None
                if not dry_run:
                    storage.set_tags(my_expnum, props)
                else:
                    sys.stdout.write(" ".join(props)+"\n")


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('field')
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

    block_name = opt.field[3]
    block_semester = opt.field[0:3]
    if opt.PREP is not None:
        opt.programs = opt.PREP

    if opt.programs is None or not len(opt.programs) > 0:
        parser.error("Must specify at least one program group to clear tags for.")

    ccds = opt.PREP is not None and [40] or ccds

    triplist = storage.open_vos_or_local('vos:OSSOS/triplets/{}_{}_discovery_expnums.txt'.format(block_name, block_semester),'r').read().split('\n')
    
    print opt.field, opt.ccd
    for tripline in triplist:
        triplets = tripline.split()
        if opt.field in triplets:
           for expnum in triplets[0:3]:
              print expnum
              clear_tags(expnum, opt.programs, ccds, dry_run=opt.dry_run)

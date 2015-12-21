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

PROGRAMS = {'CALIBRATION': ((('', 'fk'), ('mkpsf', 'zeropoint', 'fwhm', 'snr_13'), ('p', 's')),),
            'REAL': ((('',), ('step1', 'step2', 'step3', 'combine'), ('p',)),),
            'SCRAMBLE': ((('',), ('scramble', 'step1', 'step2', 'step3', 'combine'), ('s',)),),
            'PLANT': ((('',), ('plant',), ('s',)),),
            'FAKES': (('fk',), ('step1', 'step2', 'step3', 'combine'), ('s',)),
            }
PROGRAMS['FAKES'] = (PROGRAMS['FAKES'], (('',), ('astrom_mag_check',),('',)))
PREP = ((('',), ('update_header',), ('o', 'p')),)
ALL_CCDS = range(40)

logging.basicConfig(level=logging.INFO)


def check_tags(my_expnum, ops_set, my_ccds, dry_run=True):
    """
    check the tags for the given expnum/ccd set.
    @param ops:
    @param my_expnum:
    @return:
    """

    tags = storage.get_tags(my_expnum)
    count = 0
    outcount = 0
    fails = []
    for ccd in my_ccds:
      success = True
      count += 1
      for ops in ops_set:
         for fake in ops[0]:
            for my_program in ops[1]:
               for version in ops[2]:
                    # print my_expnum, fake, my_program, version, ccd
                    key = storage.get_process_tag(fake + my_program, ccd, version)
                    uri = storage.tag_uri(key)
                    if tags.get(uri, None) != storage.SUCCESS:
                        fails.append(ccd)
                        success = False
      if success:
         outcount += 1
    sys.stderr.write("{} {} {:5.1f}%\n".format(outcount, count,100* float(outcount)/count))
    return set(fails)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('block', help="Which block to check.")
    parser.add_argument('--dry-run', action='store_true')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--ccd', action='append')
    parser.add_argument('--big', action='store_true')
    group.add_argument('--PREP', dest='PREP', action='store_const', const=PREP)
    parser.add_argument('--ALL', help="Clear all processing tags except preproc and update_header",
                        dest='programs', action='store_const',
                        const=[PROGRAMS[program] for program in PROGRAMS])

    for program in PROGRAMS:
        parser.add_argument('--{}'.format(program),
                            dest='programs',
                            help="Clear tags associated with {}".format(PROGRAMS[program][0][1]),
                            action='append_const', const=PROGRAMS[program])

    opt = parser.parse_args()
    if opt.big:
	ALL_CCDS = range(40)
    else:
	ALL_CCDS = range(36)

    ccds = opt.ccd is not None and opt.ccd or ALL_CCDS

    if opt.PREP is not None:
        opt.programs = opt.PREP

    if opt.programs is None or not len(opt.programs) > 0:
        parser.error("Must specify at least one program group to clear tags for.")

    ccds = opt.PREP is not None and [36] or ccds

    block_name = opt.block[-1]
    block_semester = opt.block[:-1]

    triplist = storage.open_vos_or_local('vos:OSSOS/triplets/{}_{}_discovery_expnums.txt'.format(block_name, block_semester),'r').read().split('\n')
    

    ops = []
    for program in opt.programs:
        ops.extend(program)
    for line in triplist:
        v = line.split()
        if len(v) < 3:
            continue
        sys.stderr.write("{} ".format(v[-1]))
        field = v[3]
        if "L+0-1" in field:
            continue
        result = check_tags(v[0], ops, ccds, dry_run=opt.dry_run)
        for ccd in result:
           print line, ccd, ccd

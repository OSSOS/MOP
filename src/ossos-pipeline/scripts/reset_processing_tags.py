#!python
"""
Given an exposure number and optional ccd values clear the OSSOS processing tags associated with the expnum in dbimages.

This script is part of the process for resetting the pipeline.
"""

from ossos import storage
import logging
import argparse

PROGRAMS = {'CALIBRATION': (('', 'fk'), ('mkpsf', 'zeropoint', 'fwhm'), ('p', 's')),
            'PLANT': (('',), ('scramble', 'plant'), ('s',)),
            'SCRAMBLE': (('',), ('step1', 'step2', 'step3', 'combine'), ('s',)),
            'FAKES': (('fk',), ('step1', 'step2', 'step3', 'combine'), ('s',)),
            'DETECT': (('',), ('step1', 'step2', 'step3', 'combine'), ('p', ))}
PREP = ((('',), ('preproc', 'update_header'), ('o',)),)
ALL_CCDS = range(36)

logging.basicConfig(level=logging.DEBUG)


def clear_tags(my_expnum, ops, my_ccds):
    """
    Clear the tags for the given expnum/ccd set.
    @param ops:
    @param my_expnum:
    @return:
    """

    for fake in ops[0]:
        for my_program in ops[1]:
            for version in ops[2]:
                props = {}
                for ccd in my_ccds:
                    key = storage.get_process_tag(fake + my_program, ccd, version)
                    props[key] = None
                storage.set_tags(my_expnum, props)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('expnums', nargs='+')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--ccd', nargs=1, action='append')
    group.add_argument('--PREP', dest='programs', action='store_const', const=PREP)
    parser.add_argument('--ALL', help="Clear all processing tags except preproc and update_header",
                        dest='programs', action='store_const',
                        const=[PROGRAMS[program] for program in PROGRAMS])

    for program in PROGRAMS:
        parser.add_argument('--{}'.format(program),
                            dest='programs',
                            help="Clear tags associated with {}".format(PROGRAMS[program][1]),
                            action='append_const', const=PROGRAMS[program])

    opt = parser.parse_args()

    ccds = opt.ccd is not None and opt.ccd or range(36)
    ccds = opt.PREP is not None and 36 or ccds

    if opt.programs is None or not len(opt.programs) > 0:
        parser.error("Must specify at least one program group to clear tags for.")
    for expnum in opt.expnums:
        for program in opt.programs:
            clear_tags(expnum, program, ccds)
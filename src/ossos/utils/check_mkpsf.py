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

PROGRAMS = {'BASE': ((('',), ('mkpsf', 'step1'), ('p',)), )}


ALL_CCDS = list(range(40))

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
                    #print my_expnum, fake, my_program, version, ccd
                    key = storage.get_process_tag(fake + my_program, ccd, version)
                    uri = storage.tag_uri(key)
                    if "Failed to get image" in tags.get(uri, "Failed to get image"):
                        #print tags.get(uri, None)
                        fails.append(ccd)
                        success = False
      if success:
         outcount += 1
    sys.stderr.write("{} {} {:5.1f}%\n".format(outcount, count,100* float(outcount)/count))
    #print fails
    return set(fails)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('filename', help="filename with exposure numbers to check")
    parser.add_argument('--dry-run', action='store_true')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--ccd', action='append')
    parser.add_argument('--big', action='store_true')

    for program in PROGRAMS:
        parser.add_argument('--{}'.format(program),
                            dest='programs',
                            help="Check tags associated with {}".format(PROGRAMS[program][0][1]),
                            action='append_const', const=PROGRAMS[program])

    opt = parser.parse_args()
    if opt.big:
	ALL_CCDS = list(range(40))
    else:
	ALL_CCDS = list(range(36))

    ccds = opt.ccd is not None and opt.ccd or ALL_CCDS

    if opt.programs is None or not len(opt.programs) > 0:
        parser.error("Must specify at least one program group to clear tags for.")

    explist = open(opt.filename).readlines()
    
    ops = []
    for program in opt.programs:
        ops.extend(program)
    for expnum in explist:
        v  = expnum.split()
        if len(v) == 2:
            expnum = v[0].strip()
            ccds = [int(v[1]),]
        else:
            expnum = expnum.strip()
            if int(expnum) < 1785619:
                num_of_ccds = 36
            else:
                num_of_ccds = 40
                ccds = opt.ccd is not None and opt.ccd or list(range(num_of_ccds))
        sys.stderr.write("{}".format(expnum))
        result = check_tags(expnum, ops, ccds, dry_run=opt.dry_run)
        for ccd in result:
           print(expnum, ccd)

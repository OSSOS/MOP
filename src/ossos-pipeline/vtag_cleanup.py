__author__ = 'Michele Bannister   git:@mtbannister'

# import argparse
from glob import glob

from ossos import storage


if __name__=='__main__':
    donefiles = glob('/Users/michele/Dropbox/OSSOS/measure3/2013A-O/smonty/*.reals.astrom')
    for fn in donefiles:
        mv_file = storage.MEASURE3 + '/2013A-O/' + fn.replace('reals', 'cands').rsplit('/')[8]
        print mv_file
        storage.set_property(mv_file, 'done', 'montys')

        #undone = 'undone.txt'
        #with open('undone.txt', 'r') as infile:
        #    for expnum in infile.readlines():
        #        message = storage.get_status(expnum, 36, 'update_header_p', version='p', return_message=True)
        #
        #        # if message == None:
        #        #     with open(undone, 'a') as outfile:
        #        #         outfile.write(str(expnum))
        #        # if (message != storage.SUCCESS) and (message != None):  # let's see what happened here
        #        #     print expnum, message
        #
        #        # for fixing the bug in update_header.py at the moment
        #        storage.set_status(expnum, 36, 'update_header', message, version='p')


        # "This part never got finished (below)""
        # Query VOSpace node of an image and
    #
    # """
    # parser=argparse.ArgumentParser(
    #     description='Clean up tags on given exposures.')
    #
    # parser.add_argument("expnum",
    #                     type=int,
    #                     nargs='+',
    #                     help="Exposure numbers(s) to process")
    #
    # args=parser.parse_args()
    # ccdlist = range(0,36)
    # singlesteps = ['preproc', 'preproc_o', 'update_header', 'update_header_p']
    # steps = ['mkpsf', 'mkpsf_p', 'step1', 'step1_p', 'step2', 'step2_p', 'step3', 'step3_p', 'combine', 'combine_p',
    #          'scramble', 'scramble_s', 'plant', 'plant_s', 'mkpsf_s', 'step1_s', 'fkstep1', 'fkstep1_s',
    #          'fkstep2', 'fkstep2_s', 'fkstep3', 'fkstep3_s', 'fkcombine', 'fkcombine_s']
    #
    # for expnum in args.expnum:
    #     for step in singlesteps:


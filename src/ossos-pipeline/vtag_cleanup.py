__author__ = 'Michele Bannister   git:@mtbannister'

# import argparse
from glob import glob

from ossos import storage


if __name__=='__main__':
    user_id = 'mtb55'  # set as appropriate
    uploaded_count = 0

    donefiles = glob('/Users/michele/Dropbox/OSSOS/measure3/2013A-O/*.reals.astrom')
    for fname in donefiles:
        fn = fname.rsplit('/')[len(fname.rsplit('/')) - 1]
        vo_reals = storage.MEASURE3 + '/2013A-O/' + fn
        mv_file = storage.MEASURE3 + '/2013A-O/' + fn.replace('reals', 'cands')
        # check if file's .cands.astrom equivalent in VOSpace has a #done tag
        wasdone = storage.get_property(mv_file, 'done')
        if not wasdone:
            if not storage.exists(vo_reals):  # shouldn't possibly be there but let's just make sure
                storage.copy(fname, vo_reals)
                storage.set_property(mv_file, 'done', user_id)  # set the .cands.astrom #done tag to the user ID.
                uploaded_count += 1
        else:
            print fn, wasdone

    print 'Added unique files:', uploaded_count





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


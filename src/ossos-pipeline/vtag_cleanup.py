__author__ = 'Michele Bannister   git:@mtbannister'

from glob import glob

from ossos import storage
from ossos.gui import context
from ossos.gui import tasks
from ossos.gui.progress import DONE_PROPERTY


def update_vos_with_local_files(user_id, vos_dir, dir_to_scan):
    uploaded_count = 0
    donefiles = glob(dir_to_scan + '*.reals.astrom')
    for fname in donefiles:
        fn = fname.rsplit('/')[len(fname.rsplit('/')) - 1]
        vo_reals = vos_dir + fn
        mv_file = vos_dir + fn.replace('reals', 'cands')
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

    return


def fix_tags_on_cands_missing_reals(user_id, vos_dir, property):
    "At the moment this just checks for a single user's missing reals. Easy to generalise it to all users."
    con = context.get_context(vos_dir)
    user_progress = []
    listing = con.get_listing(tasks.get_suffix('reals'))
    mpc_listing = con.get_listing('mpc')
    for filename in listing:
        if not filename.startswith('fk'):
            user = storage.get_property(con.get_full_path(filename), property)
            if (user is not None):
                # and (user == user_id):  # modify 'and' to generalise to all users with work in this directory
                #realsfile = filename.replace('cands', 'reals')
                #if not con.exists(realsfile):
                #    print filename, 'no reals file', realsfile

                # go through the listing of .mpc files and see if any match this reals.astrom
                is_present = False
                for mpcfile in [f for f in mpc_listing if not f.startswith('fk')]:
                    if mpcfile.startswith(filename):
                        print filename, user, 'exists!', mpcfile
                        is_present = True

                if not is_present:
                    user_progress.append(filename)
                    print filename, user, 'no mpc file'
                    storage.set_property(con.get_full_path(filename), property, None)

    print 'Fixed files:', len(user_progress)

    return


if __name__ == '__main__':
    user_id = ''  # set as appropriate
    vos_dir = storage.MEASURE3 + '/2013A-O/'
    dir_to_scan = '/Users/michele/Dropbox/OSSOS/measure3/2013A-O/ptsws/'

    #update_vos_with_local_files(user_id, vos_dir, dir_to_scan)
    fix_tags_on_cands_missing_reals(user_id, vos_dir, DONE_PROPERTY)  # DONE_PROPERTY or LOCK_PROPERTY


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


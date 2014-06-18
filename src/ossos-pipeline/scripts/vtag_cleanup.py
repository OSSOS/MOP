#!python
__author__ = 'Michele Bannister   git:@mtbannister'

import collections

from ossos import storage
from ossos.gui import context
from ossos.gui import tasks
from ossos.gui.progress import DONE_PROPERTY, LOCK_PROPERTY


# def update_vos_with_local_files(user_id, vos_dir, dir_to_scan):
# uploaded_count = 0
#     donefiles = glob(dir_to_scan + '*.reals.astrom')
#     for fname in donefiles:
#         fn = fname.rsplit('/')[len(fname.rsplit('/')) - 1]
#         vo_reals = vos_dir + fn
#         mv_file = vos_dir + fn.replace('reals', 'cands')
#         # check if file's .cands.astrom equivalent in VOSpace has a #done tag
#         wasdone = storage.get_property(mv_file, 'done')
#         if not wasdone:
#             if not storage.exists(vo_reals):  # shouldn't possibly be there but let's just make sure
#                 storage.copy(fname, vo_reals)
#                 storage.set_property(mv_file, 'done', user_id)  # set the .cands.astrom #done tag to the user ID.
#                 uploaded_count += 1
#         else:
#             print fn, wasdone
#
#     print 'Added unique files:', uploaded_count
#
#     return

def what_locks_remain(directory):
    con = context.get_context(directory)
    listing = con.get_listing(tasks.get_suffix('cands'))
    user_progress = collections.defaultdict(int)

    for filename in listing:
        user = storage.get_property(con.get_full_path(filename), LOCK_PROPERTY)
        if user is not None:
            user_progress[user] += 1
            print filename, 'lock_holder=', user
            storage.set_property(con.get_full_path(filename), LOCK_PROPERTY, None)

    for user, num_locked in user_progress.iteritems():
        print "  %s: %d" % (user, num_locked)

    return

def ensure_cands_have_matching_reals(directory):
    con = context.get_context(directory)
    listing = con.get_listing(tasks.get_suffix('cands'))

    for filename in listing:
        user = storage.get_property(con.get_full_path(filename), DONE_PROPERTY)
        if user is not None:
            reals_file = filename.replace('cands', 'reals')
            if not con.exists(reals_file):
                print '.cands.astrom has no matching reals.astrom!', filename, 'done by', user

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
    vos_dir = storage.MEASURE3 + '/2013B-L/'

    what_locks_remain(vos_dir)
    ensure_cands_have_matching_reals(vos_dir)
    #update_vos_with_local_files(user_id, vos_dir, dir_to_scan)
    # fix_tags_on_cands_missing_reals(user_id, vos_dir, DONE_PROPERTY)  # DONE_PROPERTY or LOCK_PROPERTY


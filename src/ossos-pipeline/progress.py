#!/usr/bin/env python

__author__ = "David Rusk <drusk@uvic.ca>"

import argparse
import collections

from ossos import storage
from ossos.gui import context
from ossos.gui import tasks
from ossos.gui.progress import DONE_PROPERTY


def print_progress_stats(task, directory):
    con = context.get_context(directory)

    user_progress = collections.defaultdict(int)
    listing = con.get_listing(tasks.get_suffix(task))
    for filename in listing:
        user = storage.get_property(con.get_full_path(filename), DONE_PROPERTY)
        if user is not None:
            user_progress[user] += 1

    total_processed = sum(user_progress.values())

    print "%s: %s: %d of %d processed." % (
        directory, task, total_processed, len(listing))
    print "---"

    for user, num_processed in user_progress.iteritems():
        print "  %s: %d" % (user, num_processed)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("task", choices=tasks.task_list,
                        help="The task to check progress for.")
    parser.add_argument("--directory",
                        default="vos:OSSOS/measure3",
                        help="The directory to be examined for progress.")

    args = parser.parse_args()

    print_progress_stats(args.task, args.directory)


if __name__ == "__main__":
    main()

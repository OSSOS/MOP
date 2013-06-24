__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop import tasks


LOGFILE = ".pymop.log"
CANDS = "CANDS"
REALS = "REALS"

DONE_SUFFIX = ".DONE"


class ProgressManager(object):
    """
    Manages persistence of progress made processing files in a directory.
    """

    def __init__(self, directory):
        self.directory = directory

    def get_done(self, task):
        listing = tasks.listdir_for_suffix(self.directory,
                                           self._get_done_suffix(task))
        return [done_file[:-len(DONE_SUFFIX)] for done_file in listing]

    def record_done(self, filename):
        open(self._get_full_path(filename) + DONE_SUFFIX, "wb").close()

    def clean(self):
        """
        Remove all persistence-related files from the directory.
        """
        listing = tasks.listdir_for_suffix(self.directory, DONE_SUFFIX)
        for filename in listing:
            os.remove(self._get_full_path(filename))

    def _get_done_suffix(self, task):
        return tasks.suffixes[task] + DONE_SUFFIX

    def _get_full_path(self, filename):
        return os.path.join(self.directory, filename)


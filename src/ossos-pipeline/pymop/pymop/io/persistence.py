__author__ = "David Rusk <drusk@uvic.ca>"

import getpass
import os

from pymop import tasks


LOGFILE = ".pymop.log"
CANDS = "CANDS"
REALS = "REALS"

DONE_SUFFIX = ".DONE"
LOCK_SUFFIX = ".LOCK"


class FileLockedException(Exception):
    """Indicates someone already has a lock on the requested file."""
    def __init__(self, filename, locker):
        self.filename = filename
        self.locker = locker

        super(FileLockedException, self).__init__(
            "%s is locked by %s" % (filename, locker))


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

    def record_source(self, index):
        raise NotImplementedError()

    def lock(self, filename):
        lockfile = self._get_full_path(filename + LOCK_SUFFIX)

        if os.path.exists(lockfile):
            # The file is already locked
            with open(lockfile, "rb") as filehandle:
                locker = filehandle.read()

            raise FileLockedException(filename, locker)

        else:
            # The file has not been locked, we can grab it
            with open(lockfile, "wb") as filehandle:
                filehandle.write(getpass.getuser())

    def unlock(self, filename):
        lockfile = self._get_full_path(filename + LOCK_SUFFIX)

        with open(lockfile, "rb") as filehandle:
            locker = filehandle.read()
            if locker == getpass.getuser():
                # It was us who locked it
                os.remove(lockfile)
            else:
                # Can't remove someone else's lock!
                raise FileLockedException(filename, locker)

    def clean(self):
        """
        Remove all persistence-related files from the directory.
        """
        for suffix in [DONE_SUFFIX, LOCK_SUFFIX]:
            listing = tasks.listdir_for_suffix(self.directory, suffix)
            for filename in listing:
                os.remove(self._get_full_path(filename))

    def _get_done_suffix(self, task):
        return tasks.suffixes[task] + DONE_SUFFIX

    def _get_full_path(self, filename):
        return os.path.join(self.directory, filename)


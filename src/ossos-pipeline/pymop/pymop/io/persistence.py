__author__ = "David Rusk <drusk@uvic.ca>"

import getpass
import os

from pymop import tasks


LOGFILE = ".pymop.log"
CANDS = "CANDS"
REALS = "REALS"

DONE_SUFFIX = ".DONE"
LOCK_SUFFIX = ".LOCK"
PART_SUFFIX = ".PART"

INDEX_SEP = "\n"


def requires_lock(function):
    """
    Decorator to check if the user owns the required lock.
    The first argument must be the filename.
    """

    def new_lock_requiring_function(self, filename, *args, **kwargs):
        if self.owns_lock(filename):
            return function(self, filename, *args, **kwargs)
        else:
            raise RequiresLockException()

    return new_lock_requiring_function


class FileLockedException(Exception):
    """Indicates someone already has a lock on the requested file."""

    def __init__(self, filename, locker):
        self.filename = filename
        self.locker = locker

        super(FileLockedException, self).__init__(
            "%s is locked by %s" % (filename, locker))


class RequiresLockException(Exception):
    def __init__(self):
        super(RequiresLockException, self).__init__(
            "Operation requires a lock on the file.")


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

    def get_processed_indices(self, filename):
        partfile = self._get_full_path(filename + PART_SUFFIX)

        with open(partfile, "rb") as filehandle:
            indices = filehandle.read().rstrip(INDEX_SEP).split(INDEX_SEP)
            return map(int, indices)

    @requires_lock
    def record_done(self, filename):
        """
        Records a file as being completely processed.

        Args:
          filename: str
            A file in the working directory to mark as processed.
            The caller MUST own the lock on this file.

        Returns: void

        NOTE: Removes the caller's lock on the file, and removes records of
        partial results.
        """
        open(self._get_full_path(filename) + DONE_SUFFIX, "wb").close()
        self.clean(suffixes=[PART_SUFFIX, LOCK_SUFFIX])

    @requires_lock
    def record_index(self, filename, index):
        """
        Records that an item at a specific index within a file has
        been processed.

        Args:
          filename: str
            The file in the working directory which contains the index
            which is being marked as processed.
            The caller MUST own the lock on this file.
          index: int
            The 0-based index of the item that has been processed.

        Returns: void
        """
        partfile = self._get_full_path(filename + PART_SUFFIX)

        with open(partfile, "ab") as filehandle:
            filehandle.write(str(index) + INDEX_SEP)

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

        if not os.path.exists(lockfile):
            # The lock file was probably already cleaned up by record_done
            return

        with open(lockfile, "rb") as filehandle:
            locker = filehandle.read()
            if locker == getpass.getuser():
                # It was us who locked it
                os.remove(lockfile)
            else:
                # Can't remove someone else's lock!
                raise FileLockedException(filename, locker)

    def clean(self, suffixes=None):
        """
        Remove all persistence-related files from the directory.
        """
        if suffixes is None:
            suffixes = [DONE_SUFFIX, LOCK_SUFFIX, PART_SUFFIX]

        for suffix in suffixes:
            listing = tasks.listdir_for_suffix(self.directory, suffix)
            for filename in listing:
                os.remove(self._get_full_path(filename))

    def owns_lock(self, filename):
        lockfile = self._get_full_path(filename + LOCK_SUFFIX)

        if os.path.exists(lockfile):
            with open(lockfile, "rb") as filehandle:
                return getpass.getuser() == filehandle.read()
        else:
            # No lock file, so we can't have a lock
            return False

    def _get_done_suffix(self, task):
        return tasks.suffixes[task] + DONE_SUFFIX

    def _get_full_path(self, filename):
        return os.path.join(self.directory, filename)


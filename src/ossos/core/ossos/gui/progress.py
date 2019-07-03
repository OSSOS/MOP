__author__ = "David Rusk <drusk@uvic.ca>"

import collections
import threading

from .. import storage
from .. import auth
from . import tasks


CANDS = "CANDS"
REALS = "REALS"

# Constants for local progress manager
DONE_SUFFIX = ".DONE"
LOCK_SUFFIX = ".LOCK"
PART_SUFFIX = ".PART"

# Constants for VOSpace progress manager
DONE_PROPERTY = "done"
PROCESSED_INDICES_PROPERTY = "processed_indices"
LOCK_PROPERTY = "lock_holder"

# TODO: just make them both "," for consistency
INDEX_SEP = "\n"
VO_INDEX_SEP = ","


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


class AbstractProgressManager(object):
    """
    Manages persistence of progress made processing files in a directory.
    """

    def __init__(self, working_context, userid=None):
        self.working_context = working_context
        if userid is None:
            self.userid = auth.get_cadc_username()
        else:
            self.userid = userid

    def get_done(self, task):
        """
        Gets a list of the files which have already been completely
        processed for a task.

        Args:
          task: str
            The task to find files for.

        Returns:
          done_list: list(str)
            The list of filenames that have been completely processed.
        """
        raise NotImplementedError()

    def is_done(self, filename):
        """
        Checks if a file has been completely processed.

        Args:
          filename: str
            A file in the working directory to check for completion.
            No lock is required for this operation.

        Returns:
          is_done: bool
            True if all items in the file have been processed,
            False otherwise.
        """
        raise NotImplementedError()

    def get_processed_indices(self, filename):
        """
        Retrieve indices of items that have been processed in a file.

        Args:
          filename: str
            A file in the working directory to check the progress on.
            No lock is required for this operation.

        Returns:
          processed_indices: list(int)
            The indices of items in the specified field that have been
            processed already.  Returns an empty list if no items have
            been processed.
        """
        raise NotImplementedError()

    @requires_lock
    def record_done(self, filename):
        """
        Records a file as being completely processed.

        Args:
          filename: str
            A file in the working directory to mark as processed.
            The caller MUST own the lock on this file.

        Returns: void

        NOTE: Does not remove the caller's lock on the file.
        """
        self._record_done(filename)

    def _record_done(self, filename):
        raise NotImplementedError()

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
        self._record_index(filename, index)

    def _record_index(self, filename, index):
        raise NotImplementedError()

    def lock(self, filename):
        raise NotImplementedError()

    def unlock(self, filename, do_async=False):
        """
        Unlocks the file.

        Args:
          filename: str
            The file to be unlocked.
          do_async: bool
            If set to True, tries to unlock the file asynchronously, if
            possible.  This allows processing to continue if it is not
            critical that the lock be released before continuing.

        Returns: void
        """
        raise NotImplementedError()

    def clean(self, suffixes=None):
        raise NotImplementedError()

    def owns_lock(self, filename):
        raise NotImplementedError()


class VOSpaceProgressManager(AbstractProgressManager):
    def __init__(self, working_context, userid=None, track_partial_progress=False):
        """
        By default partial results are not tracked when working in VOSpace.
        get_processed_indices returns an empty list and record_index is a
        no-op.
        """
        super(VOSpaceProgressManager, self).__init__(working_context, userid=userid)

        self.track_partial_results = track_partial_progress

    def get_done(self, task):
        return [filename for filename in self.working_context.get_listing(task)
                if self.is_done(filename)]

    def is_done(self, filename):
        return storage.has_property(self._get_uri(filename), DONE_PROPERTY)

    def get_processed_indices(self, filename):
        if not self.track_partial_results:
            return []

        uri = self._get_uri(filename)
        if not storage.has_property(uri, PROCESSED_INDICES_PROPERTY):
            return []

        raw_property = storage.get_property(uri, PROCESSED_INDICES_PROPERTY)
        return list(map(int, raw_property.split(VO_INDEX_SEP)))

    def _record_done(self, filename):
        storage.set_property(self._get_uri(filename), DONE_PROPERTY,
                             self.userid)

    def _record_index(self, filename, index):
        if not self.track_partial_results:
            return

        processed_indices = self.get_processed_indices(filename)
        processed_indices.append(index)
        processed_indices = list(map(str, processed_indices))

        storage.set_property(self._get_uri(filename),
                             PROCESSED_INDICES_PROPERTY,
                             VO_INDEX_SEP.join(processed_indices))

    def lock(self, filename):
        uri = self._get_uri(filename)

        lock_holder = storage.get_property(uri, LOCK_PROPERTY)
        if lock_holder is None:
            storage.set_property(uri, LOCK_PROPERTY, self.userid)
        elif lock_holder == self.userid:
            # We already had the lock
            pass
        else:
            raise FileLockedException(filename, lock_holder)

    def unlock(self, filename, do_async=False):
        if do_async:
            threading.Thread(target=self._do_unlock, args=(filename, )).start()
        else:
            self._do_unlock(filename)

    def _do_unlock(self, filename):
        uri = self._get_uri(filename)

        lock_holder = storage.get_property(uri, LOCK_PROPERTY)
        if lock_holder is None:
            # The file isn't actually locked.  Probably already cleaned up.
            pass
        elif lock_holder == self.userid:
            # It was us who locked it
            storage.set_property(uri, LOCK_PROPERTY, None)
        else:
            # Can't remove someone else's lock!
            raise FileLockedException(filename, lock_holder)

    def clean(self, suffixes=None):
        pass

    def owns_lock(self, filename):
        lock_holder = storage.get_property(self._get_uri(filename),
                                           LOCK_PROPERTY)
        return lock_holder == self.userid

    def _get_uri(self, filename):
        return self.working_context.get_full_path(filename)


class LocalProgressManager(AbstractProgressManager):
    """
    Persists progress locally to disk.
    """

    def __init__(self, working_context, userid=None):
        super(LocalProgressManager, self).__init__(working_context, userid=userid)

    def get_done(self, task):
        listing = self.working_context.get_listing(self._get_done_suffix(task))
        return [done_file[:-len(DONE_SUFFIX)] for done_file in listing]

    def is_done(self, filename):
        return self.working_context.exists(filename + DONE_SUFFIX)

    def get_processed_indices(self, filename):
        partfile = filename + PART_SUFFIX
        donefile = filename + DONE_SUFFIX

        file_with_records = None
        if self.working_context.exists(donefile):
            file_with_records = donefile
        elif self.working_context.exists(partfile):
            file_with_records = partfile

        if file_with_records is None:
            return []

        filehandle = self.working_context.open(file_with_records)
        indices = filehandle.read().rstrip(INDEX_SEP).split(INDEX_SEP)
        filehandle.close()

        return list(map(int, indices))

    def _record_done(self, filename):
        partfile = filename + PART_SUFFIX
        donefile = filename + DONE_SUFFIX

        if self.working_context.exists(partfile):
            # By just renaming we keep the history of processed indices
            # available.
            self.working_context.rename(partfile, donefile)
        else:
            # Create a new blank file
            self.working_context.open(donefile).close()

    def _record_index(self, filename, index):
        filehandle = self.working_context.open(filename + PART_SUFFIX)
        filehandle.write(str(index) + INDEX_SEP)
        filehandle.close()

    def lock(self, filename):
        if self.owns_lock(filename):
            return

        lockfile = filename + LOCK_SUFFIX
        if self.working_context.exists(lockfile):
            locker = self.working_context.open(lockfile).read()
            raise FileLockedException(filename, locker)
        else:
            filehandle = self.working_context.open(lockfile)
            filehandle.write(self.userid)
            filehandle.close()

    def unlock(self, filename, do_async=False):
        # NOTE: locally this is fast so we don't both doing it asynchronously
        lockfile = filename + LOCK_SUFFIX

        if not self.working_context.exists(lockfile):
            # The lock file was probably already cleaned up by record_done
            return

        filehandle = self.working_context.open(lockfile)
        locker = filehandle.read()
        filehandle.close()

        if locker == self.userid:
            # It was us who locked it
            self.working_context.remove(lockfile)
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
            listing = self.working_context.get_listing(suffix)
            for filename in listing:
                self.working_context.remove(filename)

    def owns_lock(self, filename):
        lockfile = filename + LOCK_SUFFIX

        if self.working_context.exists(lockfile):
            filehandle = self.working_context.open(lockfile)
            lock_holder = filehandle.read()
            filehandle.close()
            return self.userid == lock_holder
        else:
            # No lock file, so we can't have a lock
            return False

    def _atomic_create(self, filename):
        """
        NOTE: this is not currently being used.

        Tries to create the specified file.  Throws an OSError if it already
        exists.
        """
        # fd = os.open(full_path, os.O_WRONLY | os.O_CREAT | os.O_EXCL)
        # return os.fdopen(fd, "wb")
        pass

    def _get_done_suffix(self, task):
        return tasks.get_suffix(task) + DONE_SUFFIX


class InMemoryProgressManager(AbstractProgressManager):
    """
    An implementation of the ProgressManager interface which stores all
    information in memory.  It is mainly intended for convenience in
    testing.
    """

    def __init__(self, working_context, userid=None):
        super(InMemoryProgressManager, self).__init__(working_context, userid=userid)
        self.done = set()
        self.owned_locks = set()
        self.external_locks = set()
        self.processed_indices = collections.defaultdict(list)

    def get_done(self, task):
        listing = self.working_context.get_listing(self._get_done_suffix(task))
        return [done_file[:-len(DONE_SUFFIX)] for done_file in listing]

    def is_done(self, filename):
        return filename in self.done

    def get_processed_indices(self, filename):
        return self.processed_indices[filename]

    def _record_done(self, filename):
        self.done.add(filename)

    def _record_index(self, filename, index):
        self.processed_indices[filename].append(index)

    def lock(self, filename):
        if filename in self.external_locks:
            raise FileLockedException(filename, "x")

        self.owned_locks.add(filename)

    def unlock(self, filename, do_async=False):
        if filename in self.external_locks:
            raise FileLockedException(filename, "x")

        if filename in self.owned_locks:
            self.owned_locks.remove(filename)

    def owns_lock(self, filename):
        return filename in self.owned_locks

    def add_external_lock(self, filename):
        self.external_locks.add(filename)

    def _get_done_suffix(self, task):
        return tasks.get_suffix(task) + DONE_SUFFIX

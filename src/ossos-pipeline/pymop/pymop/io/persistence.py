__author__ = "David Rusk <drusk@uvic.ca>"

import os
import json

from pymop import tasks


LOGFILE = ".pymop.log"
CANDS = "CANDS"
REALS = "REALS"

DONE_SUFFIX = ".DONE"


class PersistenceManager(object):
    """
    Manages persistence of progress made processing files in a directory.
    """

    def __init__(self, directory):
        self.directory = directory

    def get_processed(self, task):
        listing = tasks.listdir_for_suffix(self.directory,
                                           self._get_done_suffix(task))
        return [done_file[:-len(DONE_SUFFIX)] for done_file in listing]

    def record_processed(self, filename):
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


class ProgressRecord(object):
    """
    Stores the progress made processing a directory.
    """

    def __init__(self, filehandle):
        self.filehandle = filehandle

        try:
            self.records = json.load(filehandle)
        except ValueError:
            # No existing data, or it has become corrupted.
            # In either case, start from a clean slate.
            self.records = {CANDS: [], REALS: []}

    def _get_records(self, task):
        if task == tasks.CANDS_TASK:
            return self.records[CANDS]
        elif task == tasks.REALS_TASK:
            return self.records[REALS]
        else:
            raise ValueError("Unknown task: %s" % task)

    def get_processed(self, task):
        return self._get_records(task)

    def record_processed(self, filename, task):
        self._get_records(task).append(filename)

    def flush(self):
        self.filehandle.truncate(0)
        self.filehandle.seek(0)
        json.dump(self.records, self.filehandle)
        self.filehandle.flush()

    def close(self):
        self.flush()
        self.filehandle.close()


def load_progress(working_directory):
    filename = os.path.join(working_directory, LOGFILE)

    try:
        filehandle = open(filename, "r+b")
    except IOError:
        # Log file doesn't yet exist.  Don't use w+b mode right away because
        # it clears the file contents if the file does exist.
        filehandle = open(filename, "w+b")

    return ProgressRecord(filehandle)

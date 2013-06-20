__author__ = "David Rusk <drusk@uvic.ca>"

import os
import json

from pymop import tasks


LOGFILE = ".pymop.log"
CANDS = "CANDS"
REALS = "REALS"


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
        json.dump(self.records, self.filehandle)

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

__author__ = "David Rusk <drusk@uvic.ca>"

import os
import json

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
            self.records = json.loads(filehandle.read())
        except ValueError:
            # No existing data, or it has become corrupted.
            # In either case, start from a clean slate.
            self.records = {CANDS: [], REALS: []}

    def get_processed_cands_files(self):
        return self.records[CANDS]

    def get_processed_reals_files(self):
        return self.records[REALS]


def load_progress(working_directory):
    filename = os.path.join(working_directory, LOGFILE)

    try:
        filehandle = open(filename, "r+b")
    except IOError:
        # Log file doesn't yet exist.  Don't use w+b mode right away because
        # it clears the file contents if the file does exist.
        filehandle = open(filename, "w+b")

    return ProgressRecord(filehandle)

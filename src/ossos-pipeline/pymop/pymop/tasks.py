__author__ = "David Rusk <drusk@uvic.ca>"

import os

CANDS_TASK = "Process candidate objects"
REALS_TASK = "Process real objects"

task_list = [CANDS_TASK, REALS_TASK]

suffixes = {
    CANDS_TASK: ".cands.astrom",
    REALS_TASK: ".reals.astrom"
}


# TODO: remove after reworking workload loading
def listdir_for_task(working_directory, task):
    return listdir_for_suffix(working_directory, suffixes[task])


# TODO: remove after reworking workload loading
def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))


def get_suffix(task):
    return suffixes[task]

__author__ = "David Rusk <drusk@uvic.ca>"

CANDS_TASK = "cands"
REALS_TASK = "reals"
TRACK_TASK = "track"

task_list = [CANDS_TASK, REALS_TASK, TRACK_TASK]

suffixes = {
    CANDS_TASK: ".cands.astrom",
    REALS_TASK: ".reals.astrom",
    TRACK_TASK: ".mpc"
}


def get_suffix(task):
    return suffixes[task]

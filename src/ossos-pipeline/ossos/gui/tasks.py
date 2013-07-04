__author__ = "David Rusk <drusk@uvic.ca>"

CANDS_TASK = "cands"
REALS_TASK = "reals"

task_list = [CANDS_TASK, REALS_TASK]

suffixes = {
    CANDS_TASK: ".cands.astrom",
    REALS_TASK: ".reals.astrom"
}


def get_suffix(task):
    return suffixes[task]

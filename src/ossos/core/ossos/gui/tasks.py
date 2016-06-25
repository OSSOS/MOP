__author__ = "David Rusk <drusk@uvic.ca>"

CANDS_TASK = "cands"
REALS_TASK = "reals"
TRACK_TASK = "track"
TARGET_TASK = "target"

task_list = [CANDS_TASK, REALS_TASK, TRACK_TASK, TARGET_TASK]

suffixes = {
    CANDS_TASK: ".cands.astrom",
    REALS_TASK: ".reals.astrom",
    TRACK_TASK: ".ast",
    TARGET_TASK: ".ssois"
}


def get_suffix(task):
    return suffixes[task]

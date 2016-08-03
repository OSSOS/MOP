__author__ = "David Rusk <drusk@uvic.ca>"

CANDS_TASK = "cands"
REALS_TASK = "reals"
TRACK_TASK = "track"
TARGET_TASK = "target"
VETTING_TASK = "vetting"
EXAMINE_TASK = "examine"


task_list = [CANDS_TASK, REALS_TASK, TRACK_TASK, TARGET_TASK, VETTING_TASK, EXAMINE_TASK]

suffixes = {
    CANDS_TASK: ".cands.astrom",
    REALS_TASK: ".reals.astrom",
    TRACK_TASK: ".ast",
    TARGET_TASK: ".ssois",
    VETTING_TASK: ".vetting",
    EXAMINE_TASK: ".examine"
}


def get_suffix(task):
    return suffixes[task]

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from pymop import config

CANDS_TASK = "process_cands_task"
REALS_TASK = "process_reals_task"


class PymopError(Exception):
    """Base class for errors in the pymop application."""


class PymopLockError(Exception):
    """Indicates working directory cannot be locked."""

    def __init__(self, owner):
        super(PymopLockError, self).__init__()

        self.owner = owner


class ProcessCandidatesTask(object):
    pass


class ProcessRealsTask(object):
    def __init__(self):
        print "Starting process reals task"


class PymopApplication(object):
    task_name_mapping = {
        CANDS_TASK: ProcessCandidatesTask,
        REALS_TASK: ProcessRealsTask
    }

    def __init__(self):
        self.wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        wx.CallAfter(self.launch)
        self.wx_app.MainLoop()

    def launch(self):
        working_directory, task = self.run_startup_wizard()

        try:
            acquire_lock(working_directory)
        except PymopLockError as err:
            # TODO: GUI dialog
            print "Working directory already locked by %s" % err.owner

        try:
            self.start_task(task)
        except PymopError as err:
            # TODO: GUI dialog
            print "Cannot start task: %s" % err.message

    def run_startup_wizard(self):
        # TODO: Get real values using wizard
        return "/home/drusk/gitcadc/MOP/src/pymop/test/measure3", REALS_TASK

    def start_task(self, taskname):
        try:
            self.task_name_mapping[taskname]()
        except KeyError:
            raise PymopError("Unknown task: %s" % taskname)


def acquire_lock(directory):
    # TODO
    # Check for .pymop.lock file
    # If it doesn't exist, create one containing our user name
    # If one does exist, throw exception
    pass

__author__ = "David Rusk <drusk@uvic.ca>"

import os

import wx
import wx.lib.inspection

from pymop import config
from pymop.io.astrom import AstromParser
from pymop.io.mpc import MPCWriter
from pymop.io.naming import ProvisionalNameGenerator
from pymop.io.imgaccess import (AsynchronousImageDownloadManager,
                                ImageSliceDownloader, VOSpaceResolver)
from pymop.gui.models import ProcessRealsModel
from pymop.gui.controllers import ApplicationController

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
        self.parser = AstromParser()
        self.name_generator = ProvisionalNameGenerator()

        self.download_manager = AsynchronousImageDownloadManager(
            ImageSliceDownloader(VOSpaceResolver()))

    def start(self, working_directory):
        # TODO Get all .reals.astrom files in working directory
        real_astrom_file = "/home/drusk/gitcadc/MOP/src/pymop/test/data/1616681p22.measure3.cands.astrom"

        # Parse into AstromData
        astrom_data = self.parser.parse(real_astrom_file)

        # Create output file
        # TODO: check if one already exists (related to continuing existing work)
        self.output_filehandle = open("/home/drusk/gitcadc/MOP/src/pymop/test/data/reals.mpc", "wb")
        model = ProcessRealsModel(astrom_data, self.download_manager)
        output_writer = MPCWriter(self.output_filehandle)
        ApplicationController(self, model, output_writer, self.name_generator)

    def finish(self):
        self.output_filehandle.close()


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

        self.launch()

        self.wx_app.MainLoop()

    def launch(self):
        working_directory, task = self.run_startup_wizard()

        try:
            acquire_lock(working_directory)
        except PymopLockError as err:
            # TODO: GUI dialog
            print "Working directory already locked by %s" % err.owner

        try:
            self.start_task(working_directory, task)
        except PymopError as err:
            # TODO: GUI dialog
            print "Cannot start task: %s" % err.message

    def run_startup_wizard(self):
        # TODO: Get real values using wizard
        return "/home/drusk/gitcadc/MOP/src/pymop/test/measure3", REALS_TASK

    def start_task(self, working_directory, taskname):
        try:
            self.task = self.task_name_mapping[taskname]()
        except KeyError:
            raise PymopError("Unknown task: %s" % taskname)

        self.task.start(working_directory)


def acquire_lock(directory):
    # TODO
    # Check for .pymop.lock file
    # If it doesn't exist, create one containing our user name
    # If one does exist, throw exception
    pass


def listdir_for_suffix(directory, suffix):
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))


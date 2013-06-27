__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from pymop import config
from pymop import tasks
from pymop.io.workload import DirectoryManager, WorkloadManager, WorkUnitProvider, WorkUnitBuilder
from pymop.io.writers import WriterFactory
from pymop.io.astrom import AstromParser, AstromWorkload
from pymop.io.persistence import ProgressManager
from pymop.io.naming import ProvisionalNameGenerator
from pymop.io.imgaccess import (AsynchronousImageDownloadManager,
                                ImageSliceDownloader, VOSpaceResolver)
from pymop.gui.models import ProcessRealsModel, ProcessCandidatesModel
from pymop.gui.controllers import ProcessRealsController, ProcessCandidatesController
from pymop.gui.taskselect import TaskSetupManager


class PymopError(Exception):
    """Base class for errors in the pymop application."""


class PymopLockError(Exception):
    """Indicates working directory cannot be locked."""

    def __init__(self, owner):
        super(PymopLockError, self).__init__()

        self.owner = owner


class AbstractTask(object):
    def __init__(self):
        self.parser = AstromParser()
        self.download_manager = AsynchronousImageDownloadManager(
            ImageSliceDownloader(VOSpaceResolver()))

    def get_task(self):
        raise NotImplementedError()

    def _create_model(self, workload_manager):
        raise NotImplementedError()

    def _create_controller(self, model):
        raise NotImplementedError()

    def start(self, working_directory):
        directory_manager = DirectoryManager(working_directory)
        progress_manager = ProgressManager(directory_manager)
        writer_factory = WriterFactory()
        builder = WorkUnitBuilder(self.parser, writer_factory)
        workunit_provider = WorkUnitProvider(self.get_task(), directory_manager,
                                             progress_manager, builder)
        workload_manager = WorkloadManager(workunit_provider, progress_manager)
        # workload = AstromWorkload(working_directory, progress_manager,
        #                           self.get_task())
        model = self._create_model(workload_manager)
        self._create_controller(model)

    def finish(self):
        pass


class ProcessCandidatesTask(AbstractTask):
    def __init__(self):
        super(ProcessCandidatesTask, self).__init__()

    def get_task(self):
        return tasks.CANDS_TASK

    def _create_model(self, workload_manager):
        return ProcessCandidatesModel(workload_manager, self.download_manager)

    def _create_controller(self, model):
        return ProcessCandidatesController(self, model)


class ProcessRealsTask(AbstractTask):
    def __init__(self):
        super(ProcessRealsTask, self).__init__()

        self.name_generator = ProvisionalNameGenerator()

    def get_task(self):
        return tasks.REALS_TASK

    def _create_model(self, workload_manager):
        return ProcessRealsModel(workload_manager, self.download_manager)

    def _create_controller(self, model):
        return ProcessRealsController(self, model, self.name_generator)


class PymopApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTask,
        tasks.REALS_TASK: ProcessRealsTask
    }

    def __init__(self):
        self.wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        TaskSetupManager(self).run()

        self.wx_app.MainLoop()

    def set_task_info(self, working_directory, task):
        self.launch(working_directory, task)

    def launch(self, working_directory, task):
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

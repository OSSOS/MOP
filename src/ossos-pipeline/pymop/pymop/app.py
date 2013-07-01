__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from pymop import config
from pymop import tasks
from pymop.io.workload import (DirectoryManager, WorkUnitProvider,
                               RealsWorkUnitBuilder,
                               CandidatesWorkUnitBuilder)
from pymop.io.writers import WriterFactory
from pymop.io.astrom import AstromParser
from pymop.io.persistence import ProgressManager
from pymop.io.naming import ProvisionalNameGenerator
from pymop.io.imgaccess import (AsynchronousImageDownloadManager,
                                ImageSliceDownloader, VOSpaceResolver)
from pymop.gui.models import ProcessRealsModel, ProcessCandidatesModel
from pymop.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)
from pymop.gui.taskselect import TaskSetupManager


class PymopError(Exception):
    """Base class for errors in the pymop application."""


class AbstractTask(object):
    def __init__(self):
        self.parser = AstromParser()
        self.download_manager = AsynchronousImageDownloadManager(
            ImageSliceDownloader(VOSpaceResolver()))

    def get_task_suffix(self):
        raise NotImplementedError()

    def _create_model(self, workunit_provider, progress_manager):
        raise NotImplementedError()

    def _create_controller(self, model):
        raise NotImplementedError()

    def _get_workunit_builder(self, parser, writer_factory):
        raise NotImplementedError()

    def start(self, working_directory):
        directory_manager = DirectoryManager(working_directory)
        progress_manager = ProgressManager(directory_manager)
        writer_factory = WriterFactory()
        builder = self._get_workunit_builder(self.parser, progress_manager, writer_factory)
        workunit_provider = WorkUnitProvider(self.get_task_suffix(), directory_manager,
                                             progress_manager, builder)
        model = self._create_model(workunit_provider, progress_manager)
        self._create_controller(model)

    def finish(self):
        pass


class ProcessCandidatesTask(AbstractTask):
    def __init__(self):
        super(ProcessCandidatesTask, self).__init__()

    def get_task_suffix(self):
        return tasks.get_suffix(tasks.CANDS_TASK)

    def _get_workunit_builder(self, parser, progress_manager, writer_factory):
        return CandidatesWorkUnitBuilder(parser, progress_manager, writer_factory)

    def _create_model(self, workunit_provider, progress_manager):
        return ProcessCandidatesModel(workunit_provider, progress_manager, self.download_manager)

    def _create_controller(self, model):
        return ProcessCandidatesController(self, model)


class ProcessRealsTask(AbstractTask):
    def __init__(self):
        super(ProcessRealsTask, self).__init__()

        self.name_generator = ProvisionalNameGenerator()

    def get_task_suffix(self):
        return tasks.get_suffix(tasks.REALS_TASK)

    def _get_workunit_builder(self, parser, progress_manager, writer_factory):
        return RealsWorkUnitBuilder(parser, progress_manager, writer_factory)

    def _create_model(self, workunit_provider, progress_manager):
        return ProcessRealsModel(workunit_provider, progress_manager, self.download_manager)

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


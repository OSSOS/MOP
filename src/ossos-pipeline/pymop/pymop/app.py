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
from pymop.gui.models import UIModel
from pymop.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)
from pymop.gui.taskselect import TaskSetupManager


class PymopError(Exception):
    """Base class for errors in the pymop application."""


class AbstractTaskFactory(object):
    def create_workunit_builder(self, parser, progress_manager, writer_factory):
        pass

    def create_controller(self, model):
        pass


class ProcessRealsTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self, parser, progress_manager, writer_factory):
        return RealsWorkUnitBuilder(parser, progress_manager, writer_factory)

    def create_controller(self, model):
        return ProcessRealsController(model, ProvisionalNameGenerator())


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self, parser, progress_manager, writer_factory):
        return CandidatesWorkUnitBuilder(parser, progress_manager, writer_factory)

    def create_controller(self, model):
        return ProcessCandidatesController(model)


class PymopApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTaskFactory,
        tasks.REALS_TASK: ProcessRealsTaskFactory
    }

    def __init__(self):
        self.wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        TaskSetupManager(self).run()

        self.wx_app.MainLoop()

    def start_task(self, working_directory, taskname):
        try:
            factory = self.task_name_mapping[taskname]()
        except KeyError:
            raise PymopError("Unknown task: %s" % taskname)

        parser = AstromParser()
        download_manager = AsynchronousImageDownloadManager(
            ImageSliceDownloader(VOSpaceResolver()))

        directory_manager = DirectoryManager(working_directory)
        progress_manager = ProgressManager(directory_manager)
        writer_factory = WriterFactory()
        builder = factory.create_workunit_builder(parser, progress_manager, writer_factory)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(taskname), directory_manager,
                                             progress_manager, builder)
        model = UIModel(workunit_provider, progress_manager, download_manager)
        factory.create_controller(model)

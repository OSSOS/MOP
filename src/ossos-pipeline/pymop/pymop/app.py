__author__ = "David Rusk <drusk@uvic.ca>"

import os

import wx
import wx.lib.inspection

from pymop import config
from pymop import tasks
from pymop.io.workload import (WorkUnitProvider,
                               RealsWorkUnitBuilder,
                               CandidatesWorkUnitBuilder)
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
    def create_workunit_builder(self, parser, progress_manager):
        pass

    def create_controller(self, model):
        pass


class ProcessRealsTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def create_controller(self, model):
        return ProcessRealsController(model, ProvisionalNameGenerator())


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

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

        directory_context = DirectoryContext(working_directory)
        progress_manager = ProgressManager(directory_context)
        builder = factory.create_workunit_builder(parser, progress_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(taskname), directory_context,
                                             progress_manager, builder)
        model = UIModel(workunit_provider, progress_manager, download_manager)
        factory.create_controller(model)


class DirectoryContext(object):
    def __init__(self, directory):
        self.directory = directory

    def get_listing(self, suffix):
        return listdir_for_suffix(self.directory, suffix)

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)

    def get_file_size(self, filename):
        return os.stat(self.get_full_path(filename)).st_size


def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))

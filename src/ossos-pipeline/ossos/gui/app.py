__author__ = "David Rusk <drusk@uvic.ca>"

import os

import wx
import wx.lib.inspection

from ossos.gui import config, tasks
from ossos.gui.workload import (WorkUnitProvider,
                                RealsWorkUnitBuilder,
                                CandidatesWorkUnitBuilder)
from ossos.astrom import AstromParser
from ossos.gui.persistence import ProgressManager
from ossos.naming import ProvisionalNameGenerator
from ossos.gui.errorhandling import DownloadErrorHandler
from ossos.gui.downloads import (AsynchronousImageDownloadManager,
                                 ImageSliceDownloader)
from ossos.gui.models import UIModel
from ossos.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)


class AbstractTaskFactory(object):
    def create_workunit_builder(self, parser, progress_manager):
        pass

    def create_controller(self, model):
        pass


class ProcessRealsTaskFactory(AbstractTaskFactory):
    def __init__(self):
        # NOTE: Force expensive loading of libraries up front.  These are
        # libraries that the reals task needs but the candidates task
        # doesn't.  To make sure the candidates task doesn't load them, we
        # import them directly in the functions/methods where they are used.
        # TODO: find out what the best practice is for handling this sort of
        # situation and refactor.
        from pyraf import iraf

    def create_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def create_controller(self, model):
        return ProcessRealsController(model, ProvisionalNameGenerator())


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

    def create_controller(self, model):
        return ProcessCandidatesController(model)


class ValidationApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTaskFactory,
        tasks.REALS_TASK: ProcessRealsTaskFactory
    }

    def __init__(self, taskname, working_directory):
        wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        try:
            factory = self.task_name_mapping[taskname]()
        except KeyError:
            raise ValueError("Unknown task: %s" % taskname)

        parser = AstromParser()
        error_handler = DownloadErrorHandler(self)
        downloader = ImageSliceDownloader()
        download_manager = AsynchronousImageDownloadManager(downloader,
                                                            error_handler)

        directory_context = DirectoryContext(working_directory)
        progress_manager = ProgressManager(directory_context)
        builder = factory.create_workunit_builder(parser, progress_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(taskname), directory_context,
                                             progress_manager, builder)
        model = UIModel(workunit_provider, progress_manager, download_manager)
        controller = factory.create_controller(model)

        self.model = model
        self.view = controller.get_view()

        wx_app.MainLoop()

    def get_model(self):
        return self.model

    def get_view(self):
        return self.view


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

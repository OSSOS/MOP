__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from ossos.gui import config, tasks, logger
from ossos.gui import context
from ossos.gui.sync import SynchronizationManager
from ossos.gui.workload import (WorkUnitProvider,
                                RealsWorkUnitBuilder,
                                CandidatesWorkUnitBuilder)
from ossos.astrom import AstromParser
from ossos.naming import ProvisionalNameGenerator
from ossos.gui.errorhandling import DownloadErrorHandler
from ossos.gui.downloads import (AsynchronousImageDownloadManager,
                                 ImageSliceDownloader)
from ossos.gui.models import UIModel
from ossos.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)


class AbstractTaskFactory(object):
    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
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

    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        return RealsWorkUnitBuilder(
            parser, input_context, output_context, progress_manager)

    def create_controller(self, model):
        return ProcessRealsController(model, ProvisionalNameGenerator())


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        return CandidatesWorkUnitBuilder(
            parser, input_context, output_context, progress_manager)

    def create_controller(self, model):
        return ProcessCandidatesController(model)


class ValidationApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTaskFactory,
        tasks.REALS_TASK: ProcessRealsTaskFactory
    }

    def __init__(self, taskname, working_directory, output_directory):
        logger.info("Starting %s task in %s" % (taskname, working_directory))
        logger.info("Output directory set to: %s" % output_directory)

        wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        try:
            factory = self.task_name_mapping[taskname]()
        except KeyError:
            error_message = "Unknown task: %s" % taskname
            logger.critical(error_message)
            raise ValueError(error_message)

        parser = AstromParser()
        error_handler = DownloadErrorHandler(self)
        downloader = ImageSliceDownloader()
        download_manager = AsynchronousImageDownloadManager(downloader,
                                                            error_handler)

        working_context = context.get_context(working_directory)
        output_context = context.get_context(output_directory)

        progress_manager = working_context.get_progress_manager()
        builder = factory.create_workunit_builder(parser,
                                                  working_context,
                                                  output_context,
                                                  progress_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(taskname), working_context,
                                             progress_manager, builder)

        if working_context.is_remote():
            synchronization_manager = SynchronizationManager(working_context)
        else:
            synchronization_manager = None

        model = UIModel(workunit_provider, progress_manager, download_manager,
                        synchronization_manager)
        controller = factory.create_controller(model)

        model.start_work()

        self.model = model
        self.view = controller.get_view()

        wx_app.MainLoop()

    def get_model(self):
        return self.model

    def get_view(self):
        return self.view


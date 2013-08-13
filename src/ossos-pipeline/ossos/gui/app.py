__author__ = "David Rusk <drusk@uvic.ca>"

import sys

import wx
import wx.lib.inspection

from ossos.astrom import AstromParser
from ossos.downloads.async import AsynchronousImageDownloadManager
from ossos.downloads.cutouts import ImageCutoutDownloader
from ossos.gui import config, tasks, logger
from ossos.gui import context
from ossos.gui.sync import SynchronizationManager
from ossos.gui.workload import (WorkUnitProvider,
                                RealsWorkUnitBuilder,
                                CandidatesWorkUnitBuilder,
                                PreFetchingWorkUnitProvider)
from ossos.gui.errorhandling import DownloadErrorHandler
from ossos.gui.models import TransAckUIModel
from ossos.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)
from ossos.naming import ProvisionalNameGenerator, DryRunNameGenerator


class AbstractTaskFactory(object):
    def __init__(self, dryrun=False):
        self.dryrun = dryrun

    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        pass

    def create_controller(self, model):
        pass

    def should_randomize_workunits(self):
        pass


class ProcessRealsTaskFactory(AbstractTaskFactory):
    def __init__(self, dryrun=False):
        super(ProcessRealsTaskFactory, self).__init__(dryrun=dryrun)

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
        if self.dryrun:
            name_generator = DryRunNameGenerator()
        else:
            name_generator = ProvisionalNameGenerator()

        return ProcessRealsController(model, name_generator)

    def should_randomize_workunits(self):
        return False


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def __init__(self, dryrun=False):
        super(ProcessCandidatesTaskFactory, self).__init__(dryrun=dryrun)

    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        return CandidatesWorkUnitBuilder(
            parser, input_context, output_context, progress_manager)

    def create_controller(self, model):
        return ProcessCandidatesController(model)

    def should_randomize_workunits(self):
        return True


class ValidationApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTaskFactory,
        tasks.REALS_TASK: ProcessRealsTaskFactory
    }

    def __init__(self, taskname, working_directory, output_directory,
                 dryrun=False):
        logger.info("Starting %s task in %s" % (taskname, working_directory))
        logger.info("Output directory set to: %s" % output_directory)

        wx_app = wx.App(False)

        debug_mode = config.read("DEBUG")
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        try:
            factory = self.task_name_mapping[taskname](dryrun=dryrun)
        except KeyError:
            error_message = "Unknown task: %s" % taskname
            logger.critical(error_message)
            raise ValueError(error_message)

        parser = AstromParser()
        error_handler = DownloadErrorHandler(self)
        downloader = ImageCutoutDownloader()
        download_manager = AsynchronousImageDownloadManager(downloader,
                                                            error_handler)

        working_context = context.get_context(working_directory)
        output_context = context.get_context(output_directory)

        if dryrun and working_context.is_remote():
            sys.stdout.write("A dry run can only be done on local files.\n")
            sys.exit(0)

        if output_context.is_remote():
            sys.stdout.write("The output directory must be local.\n")
            sys.exit(0)

        progress_manager = working_context.get_progress_manager()
        builder = factory.create_workunit_builder(parser,
                                                  working_context,
                                                  output_context,
                                                  progress_manager)

        workunit_provider = WorkUnitProvider(tasks.get_suffix(taskname),
                                             working_context,
                                             progress_manager, builder,
                                             randomize=factory.should_randomize_workunits())

        prefetching_workunit_provider = PreFetchingWorkUnitProvider(workunit_provider, 1)

        if working_context.is_remote():
            synchronization_manager = SynchronizationManager(working_context)
        else:
            synchronization_manager = None

        model = TransAckUIModel(prefetching_workunit_provider,
                                download_manager,
                                synchronization_manager)
        logger.debug("Created model.")

        controller = factory.create_controller(model)
        logger.debug("Created controller.")

        model.start_work()
        controller.display_current_image()

        self.model = model
        self.view = controller.get_view()

        if not synchronization_manager:
            self.view.disable_sync_menu()

        wx_app.MainLoop()

    def get_model(self):
        return self.model

    def get_view(self):
        return self.view


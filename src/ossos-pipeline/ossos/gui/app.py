__author__ = "David Rusk <drusk@uvic.ca>"

import sys

import wx
import wx.lib.inspection

from ossos.astrom import AstromParser
from ossos.downloads.async import AsynchronousDownloadManager
from ossos.downloads.cutouts.downloader import ImageCutoutDownloader
from ossos.gui import config, tasks, logger
from ossos.gui import context
from ossos.gui.sync import SynchronizationManager
from ossos.gui.workload import (WorkUnitProvider,
                                RealsWorkUnitBuilder,
                                CandidatesWorkUnitBuilder,
                                PreFetchingWorkUnitProvider)
from ossos.gui.errorhandling import DownloadErrorHandler
from ossos.gui.controllers import (ProcessRealsController,
                                   ProcessCandidatesController)
from ossos.gui.models.imagemanager import ImageManager
from ossos.gui.models.transactions import TransAckValidationModel
from ossos.gui.views.app import ApplicationView
from ossos.naming import ProvisionalNameGenerator, DryRunNameGenerator


class AbstractTaskFactory(object):
    def __init__(self, dry_run=False):
        self.dry_run = dry_run

    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        pass

    def create_controller_factory(self, model):
        pass

    def should_randomize_workunits(self):
        pass


class ProcessRealsTaskFactory(AbstractTaskFactory):
    def __init__(self, dry_run=False):
        super(ProcessRealsTaskFactory, self).__init__(dry_run=dry_run)

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
            parser, input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def create_controller_factory(self, model):
        return RealsControllerFactory(model)

    def should_randomize_workunits(self):
        return False


class ProcessCandidatesTaskFactory(AbstractTaskFactory):
    def __init__(self, dry_run=False):
        super(ProcessCandidatesTaskFactory, self).__init__(dry_run=dry_run)

    def create_workunit_builder(self,
                                parser,
                                input_context,
                                output_context,
                                progress_manager):
        return CandidatesWorkUnitBuilder(
            parser, input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def create_controller_factory(self, model):
        return CandidatesControllerFactory(model)

    def should_randomize_workunits(self):
        return True


class ControllerFactory(object):
    def __init__(self, model, dry_run=False):
        self.model= model
        self.dry_run = dry_run

    def create_controller(self, view):
        raise NotImplementedError()


class CandidatesControllerFactory(ControllerFactory):
    def create_controller(self, view):
        return ProcessCandidatesController(self.model, view)


class RealsControllerFactory(ControllerFactory):
    def create_controller(self, view):
        if self.dry_run:
            name_generator = DryRunNameGenerator()
        else:
            name_generator = ProvisionalNameGenerator()

        return ProcessRealsController(self.model, view, name_generator)


class ValidationApplication(object):
    task_name_mapping = {
        tasks.CANDS_TASK: ProcessCandidatesTaskFactory,
        tasks.REALS_TASK: ProcessRealsTaskFactory
    }

    def __init__(self, taskname, working_directory, output_directory,
                 dry_run=False, debug=False):
        logger.info("Starting %s task in %s" % (taskname, working_directory))
        logger.info("Output directory set to: %s" % output_directory)

        wx_app = wx.App(False)

        try:
            factory = self.task_name_mapping[taskname](dry_run=dry_run)
        except KeyError:
            error_message = "Unknown task: %s" % taskname
            logger.critical(error_message)
            raise ValueError(error_message)

        parser = AstromParser()
        error_handler = DownloadErrorHandler(self)

        def read(slice_config):
            return config.read("CUTOUTS.%s" % slice_config)

        singlet_downloader = ImageCutoutDownloader(
            slice_rows=read("SINGLETS.SLICE_ROWS"),
            slice_cols=read("SINGLETS.SLICE_COLS"))

        singlet_download_manager = AsynchronousDownloadManager(
            singlet_downloader, error_handler)

        triplet_downloader = ImageCutoutDownloader(
            slice_rows=read("TRIPLETS.SLICE_ROWS"),
            slice_cols=read("TRIPLETS.SLICE_COLS"))

        triplet_download_manager = AsynchronousDownloadManager(
            triplet_downloader, error_handler)

        image_manager = ImageManager(singlet_download_manager,
                                     triplet_download_manager)

        working_context = context.get_context(working_directory)
        output_context = context.get_context(output_directory)

        if dry_run and working_context.is_remote():
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

        model = TransAckValidationModel(prefetching_workunit_provider,
                                        image_manager,
                                        synchronization_manager)
        logger.debug("Created model.")

        view = ApplicationView(factory.create_controller_factory(model))
        logger.debug("Created controller.")

        model.start_work()

        self.model = model
        self.view = view
        self.controller = view.controller

        self.controller.display_current_image()

        if not synchronization_manager:
            self.view.disable_sync_menu()

        if debug:
            wx.lib.inspection.InspectionTool().Show()

        wx_app.MainLoop()

    def get_model(self):
        return self.model

    def get_view(self):
        return self.view


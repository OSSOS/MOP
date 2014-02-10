__author__ = "David Rusk <drusk@uvic.ca>"

import sys

from ossos.astrom import AstromParser
from ossos.downloads.async import AsynchronousDownloadManager
from ossos.downloads.cutouts.downloader import ImageCutoutDownloader
from ossos.gui import config, tasks, logger
from ossos.gui import context
from ossos.gui.sync import SynchronizationManager
from ossos.gui.models.workload import (WorkUnitProvider,
                                       RealsWorkUnitBuilder,
                                       TracksWorkUnitBuilder,
                                       CandidatesWorkUnitBuilder,
                                       PreFetchingWorkUnitProvider)
from ossos.gui.errorhandling import DownloadErrorHandler
from ossos.gui.controllers import (ProcessTracksController,
                                   ProcessRealsController,
                                   ProcessCandidatesController)
from ossos.gui.models.imagemanager import ImageManager
from ossos.gui.models.transactions import TransAckValidationModel
from ossos.gui.views.appview import ApplicationView
from ossos.naming import ProvisionalNameGenerator, DryRunNameGenerator
from ossos.ssos import TracksParser


def create_application(taskname, working_directory, output_directory,
                       dry_run=False, debug=False):
    logger.info("Starting %s task." % taskname)

    if taskname == tasks.CANDS_TASK:
        ProcessCandidatesApplication(working_directory, output_directory,
                                     dry_run=dry_run, debug=debug)
    elif taskname == tasks.REALS_TASK:
        ProcessRealsApplication(working_directory, output_directory,
                                dry_run=dry_run, debug=debug)
    elif taskname == tasks.TRACK_TASK:
        ProcessTracksApplication(working_directory, output_directory,
                                 dry_run=dry_run, debug=debug)
    else:
        error_message = "Unknown task: %s" % taskname
        logger.critical(error_message)
        raise ValueError(error_message)


class ValidationApplication(object):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False):
        self.dry_run = dry_run

        logger.info("Input directory set to: %s" % working_directory)
        logger.info("Output directory set to: %s" % output_directory)

        working_context = context.get_context(working_directory)
        output_context = context.get_context(output_directory)

        if dry_run and working_context.is_remote():
            sys.stdout.write("A dry run can only be done on local files.\n")
            sys.exit(0)

        if output_context.is_remote():
            sys.stdout.write("The output directory must be local.\n")
            sys.exit(0)

        image_manager = self._create_image_manager()

        progress_manager = working_context.get_progress_manager()
        builder = self._create_workunit_builder(working_context,
                                                output_context,
                                                progress_manager)

        workunit_provider = WorkUnitProvider(self.input_suffix,
                                             working_context,
                                             progress_manager, builder,
                                             randomize=self.should_randomize_workunits)

        prefetching_workunit_provider = PreFetchingWorkUnitProvider(workunit_provider,
                                                                    config.read("PREFETCH.NUMBER"),
                                                                    image_manager)

        if working_context.is_remote():
            synchronization_manager = SynchronizationManager(working_context)
        else:
            synchronization_manager = None

        model = TransAckValidationModel(prefetching_workunit_provider,
                                        image_manager,
                                        synchronization_manager)
        logger.debug("Created model.")

        view = self._create_view(model, debug=debug)

        logger.debug("Created view.")
        model.start_work()

        self.model = model
        self.view = view
        self.controller = view.controller

        self.controller.display_current_image()

        if not synchronization_manager:
            self.view.disable_sync_menu()

        self.view.show()

    def _create_image_manager(self):
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

        return ImageManager(singlet_download_manager, triplet_download_manager)

    def get_model(self):
        return self.model

    def get_view(self):
        return self.view

    @property
    def input_suffix(self):
        raise NotImplementedError()

    @property
    def should_randomize_workunits(self):
        raise NotImplementedError()

    def _create_view(self, model, debug=False):
        return ApplicationView(self._create_controller_factory(model),
                               debug=debug)

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        raise NotImplementedError()

    def _create_controller_factory(self, model):
        raise NotImplementedError()


class ProcessCandidatesApplication(ValidationApplication):
    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.CANDS_TASK]

    @property
    def should_randomize_workunits(self):
        return True

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return CandidatesWorkUnitBuilder(
            AstromParser(), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return CandidatesControllerFactory(model, dry_run=self.dry_run)


class ProcessRealsApplication(ValidationApplication):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False):
        preload_iraf()

        super(ProcessRealsApplication, self).__init__(
            working_directory, output_directory, dry_run=dry_run, debug=debug)

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.REALS_TASK]

    @property
    def should_randomize_workunits(self):
        return True

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return RealsWorkUnitBuilder(
            AstromParser(), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return RealsControllerFactory(model, dry_run=self.dry_run)


class ProcessTracksApplication(ValidationApplication):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False):
        preload_iraf()

        super(ProcessTracksApplication, self).__init__(
            working_directory, output_directory, dry_run=dry_run, debug=debug)

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.TRACK_TASK]

    @property
    def should_randomize_workunits(self):
        return False

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return TracksWorkUnitBuilder(
            TracksParser(), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return TracksControllerFactory(model, dry_run=self.dry_run)

    def _create_view(self, model, debug=False):
        return ApplicationView(self._create_controller_factory(model),
                               track_mode=True, debug=debug)


class ControllerFactory(object):
    """
    Allows the view to create the controller without direct knowledge
    of the model.
    """

    def __init__(self, model, dry_run=False):
        self.model = model
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


class TracksControllerFactory(ControllerFactory):
    def create_controller(self, view):
        if self.dry_run:
            name_generator = DryRunNameGenerator()
        else:
            name_generator = ProvisionalNameGenerator()

        return ProcessTracksController(self.model, view, name_generator)


def preload_iraf():
    logger.info("Preloading IRAF")

    # NOTE: Force expensive loading of libraries up front.  These are
    # libraries that the reals task needs but the candidates task
    # doesn't.  To make sure the candidates task doesn't load them, we
    # import them directly in the functions/methods where they are used.
    # TODO: find out what the best practice is for handling this sort of
    # situation and refactor.


__author__ = "David Rusk <drusk@uvic.ca>"

import sys
from .. import storage
from ..astrom import AstromParser, StationaryParser
from ..downloads.async_download import AsynchronousDownloadManager
from ..downloads.cutouts.downloader import ImageCutoutDownloader
from ..gui import config, tasks, logger
from ..gui import context
from ..gui.controllers import (ProcessTracksController,
                               ProcessRealsController,
                               ProcessCandidatesController, ProcessVettingController, ProcessExamineController)
from ..gui.errorhandling import DownloadErrorHandler
from ..gui.models.imagemanager import ImageManager
from ..gui.models.transactions import TransAckValidationModel
from ..gui.models.workload import (WorkUnitProvider,
                                   RealsWorkUnitBuilder,
                                   TracksWorkUnitBuilder,
                                   CandidatesWorkUnitBuilder,
                                   VettingWorkUnitBuilder,
                                   ExamineWorkUnitBuilder,
                                   PreFetchingWorkUnitProvider)
from ..gui.sync import SynchronizationManager
from ..gui.views.appview import ApplicationView
from ..naming import ProvisionalNameGenerator, DryRunNameGenerator
from ..ssos import TracksParser, TrackTarget


def create_application(task_name, working_directory, output_directory,
                       dry_run=False, debug=False, name_filter=None, user_id=None,
                       skip_previous=False, zoom=1, telescope='Subaru/SuprimeCam',
                       measure3=storage.MEASURE3):
    logger.info("Starting %s task." % task_name)

    if task_name == tasks.CANDS_TASK:
        ProcessCandidatesApplication(working_directory, output_directory,
                                     dry_run=dry_run, debug=debug, name_filter=name_filter, user_id=user_id, zoom=zoom)
    elif task_name == tasks.REALS_TASK:
        ProcessRealsApplication(working_directory, output_directory,
                                dry_run=dry_run, debug=debug, name_filter=name_filter, user_id=user_id, zoom=zoom,
                                measure3=measure3)
    elif task_name == tasks.VETTING_TASK:
        ProcessVettingApplication(working_directory, output_directory, dry_run=dry_run,
                                  debug=debug, name_filter=name_filter, user_id=user_id, zoom=zoom)
    elif task_name == tasks.EXAMINE_TASK:
        ProcessExamineApplication(working_directory, output_directory, dry_run=dry_run,
                                  debug=debug, name_filter=name_filter, user_id=user_id, zoom=zoom)
    elif task_name == tasks.TRACK_TASK:
        ProcessTracksApplication(working_directory, output_directory,
                                 dry_run=dry_run, debug=debug, name_filter=name_filter,
                                 skip_previous=skip_previous, user_id=user_id, zoom=zoom, telescope=telescope)
    elif task_name == tasks.TARGET_TASK:
        ProcessTargetApplication(working_directory, output_directory,
                                 dry_run=dry_run, debug=debug, name_filter=name_filter,
                                 skip_previous=skip_previous, user_id=user_id, zoom=zoom, telescope=telescope)
    else:
        error_message = "Unknown task: %s" % task_name
        logger.critical(error_message)
        raise ValueError(error_message)


class ValidationApplication(object):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False, name_filter=None, user_id=None, mark_using_pixels=True, zoom=1,
                 telescope='CFHT/MegaCam'):

        self.dry_run = dry_run
        self.user_id = user_id
        logger.info("Input directory set to: %s" % working_directory)
        logger.info("Output directory set to: %s" % output_directory)

        working_context = context.get_context(working_directory, userid=self.user_id)
        output_context = context.get_context(output_directory, userid=self.user_id)

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
                                             randomize=self.should_randomize_workunits,
                                             name_filter=name_filter)

        prefetching_workunit_provider = PreFetchingWorkUnitProvider(workunit_provider,
                                                                    config.read("PREFETCH.NUMBER"),
                                                                    image_manager)

        if working_context.is_remote():
            synchronization_manager = SynchronizationManager(working_context, sync_enabled=True)
        else:
            synchronization_manager = None

        model = TransAckValidationModel(prefetching_workunit_provider,
                                        image_manager,
                                        synchronization_manager)
        logger.debug("Created model.")

        view = self._create_view(model, debug=debug, mark_using_pixels=mark_using_pixels, zoom=zoom)

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

    def _create_view(self, model, debug=False, mark_using_pixels=False, zoom=1):
        return ApplicationView(self._create_controller_factory(model),
                               debug=debug,
                               mark_using_pixels=mark_using_pixels, zoom=zoom)

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
                 dry_run=False, debug=False, name_filter=None, user_id=None, zoom=1, measure3=storage.MEASURE3):
        preload_iraf()
        self._measure3 = storage.MEASURE3
        super(ProcessRealsApplication, self).__init__(
            working_directory, output_directory, dry_run=dry_run,
            debug=debug, name_filter=name_filter, user_id=user_id, mark_using_pixels=False, zoom=zoom)

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.REALS_TASK]

    @property
    def should_randomize_workunits(self):
        return True  # now we are only going to manually measure the genuine candidates not also the planted ones

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return RealsWorkUnitBuilder(
            AstromParser(), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return RealsControllerFactory(model, dry_run=self.dry_run, measure3=self._measure3)


class ProcessVettingApplication(ProcessCandidatesApplication):

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.VETTING_TASK]

    @property
    def should_randomize_workunits(self):
        return False  # now we are only going to manually measure the genuine candidates not also the planted ones

    def _create_controller_factory(self, model):
        return VettingControllerFactory(model, dry_run=self.dry_run)

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return VettingWorkUnitBuilder(StationaryParser(), input_context, output_context, progress_manager,
                                      dry_run=self.dry_run)


class ProcessExamineApplication(ProcessRealsApplication):

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.EXAMINE_TASK]

    @property
    def should_randomize_workunits(self):
        return False  # now we are only going to manually measure the genuine candidates not also the planted ones

    def _create_controller_factory(self, model):
        return ExamineControllerFactory(model, dry_run=self.dry_run)

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return ExamineWorkUnitBuilder(StationaryParser(discovery_only=False),
                                      input_context, output_context,
                                      progress_manager,
                                      dry_run=self.dry_run)


class ProcessTracksApplication(ValidationApplication):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False, name_filter=None, skip_previous=False,
                 user_id=None, zoom=1, telescope='Subaru/SuprimeCam'):
        preload_iraf()
        self.skip_previous = skip_previous
        self.telescope = telescope

        super(ProcessTracksApplication, self).__init__(
            working_directory, output_directory, dry_run=dry_run, debug=debug, 
            name_filter=name_filter,
            user_id=user_id, zoom=zoom, telescope=telescope)

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
            TracksParser(skip_previous=self.skip_previous, telescope=self.telescope), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return TracksControllerFactory(model, dry_run=self.dry_run)

    def _create_view(self, model, debug=False, mark_using_pixels=False, zoom=1):
        return ApplicationView(self._create_controller_factory(model),
                               track_mode=True, debug=debug, zoom=zoom)


class ProcessTargetApplication(ProcessTracksApplication):
    def __init__(self, working_directory, output_directory,
                 dry_run=False, debug=False, name_filter=None, skip_previous=False,
                 user_id=None, zoom=1, telescope='Subaru/SuprimeCam'):
        preload_iraf()
        self.skip_previous = skip_previous
        super(ProcessTargetApplication, self).__init__(
            working_directory, output_directory, dry_run=dry_run, debug=debug,
            name_filter=name_filter,
            user_id=user_id, zoom=zoom)
        self.controller.is_discovery = False

    @property
    def input_suffix(self):
        return tasks.suffixes[tasks.TARGET_TASK]

    @property
    def should_randomize_workunits(self):
        return False

    def _create_workunit_builder(self,
                                 input_context,
                                 output_context,
                                 progress_manager):
        return TracksWorkUnitBuilder(
            TrackTarget(skip_previous=self.skip_previous), input_context, output_context, progress_manager,
            dry_run=self.dry_run)

    def _create_controller_factory(self, model):
        return TracksControllerFactory(model, dry_run=self.dry_run)

    def _create_view(self, model, debug=False, mark_using_pixels=False, zoom=1):
        return ApplicationView(self._create_controller_factory(model),
                               track_mode=True, debug=debug, zoom=zoom)


class ControllerFactory(object):
    """
    Allows the view to create the controller without direct knowledge
    of the model.
    """

    def __init__(self, model, dry_run=False, measure3=storage.MEASURE3):
        self.model = model
        self.dry_run = dry_run
        self._measure3 = measure3

    def create_controller(self, view):
        raise NotImplementedError()


class CandidatesControllerFactory(ControllerFactory):
    def create_controller(self, view):
        return ProcessCandidatesController(self.model, view)


class VettingControllerFactory(ControllerFactory):
    def create_controller(self, view):
        return ProcessVettingController(self.model, view)


class ExamineControllerFactory(ControllerFactory):

    def create_controller(self, view):

        return ProcessExamineController(self.model, view, name_generator=ProvisionalNameGenerator())


class RealsControllerFactory(ControllerFactory):
    def create_controller(self, view):
        if self.dry_run:
            name_generator = DryRunNameGenerator()
        else:
            name_generator = ProvisionalNameGenerator(measure3=self._measure3)

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

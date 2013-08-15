__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui import events
from ossos.gui import logger
from ossos.gui.models.exceptions import (ImageNotLoadedException,
                                         NoWorkUnitException)
from ossos.gui.workload import (NoAvailableWorkException, StatefulCollection,
                                CandidatesWorkUnit, RealsWorkUnit)


class ValidationModel(object):
    """
    Contains the data and associated operations available to the user interface.
    """

    def __init__(self, workunit_provider, image_manager, synchronization_manager):
        self.workunit_provider = workunit_provider
        self.image_manager = image_manager
        self.synchronization_manager = synchronization_manager

        self.work_units = StatefulCollection()

        self.num_processed = 0

        self.sources_discovered = set()

        self.image_state = SingletState(self)

    def get_working_directory(self):
        return self.workunit_provider.directory

    def start_work(self):
        logger.debug("Model starting work.")
        self.next_workunit()

    def next_source(self):
        self.get_current_workunit().next_source()
        self.expect_source_transition()

    def previous_source(self):
        self.get_current_workunit().previous_source()
        self.expect_source_transition()

    def next_obs(self):
        self.get_current_workunit().next_obs()
        self.expect_observation_transition()

    def previous_obs(self):
        self.get_current_workunit().previous_obs()
        self.expect_observation_transition()

    def next_item(self):
        if self.get_current_workunit().is_finished():
            self.next_workunit()
        else:
            self.get_current_workunit().next_item()

            if self.is_processing_candidates():
                self.expect_source_transition()
            elif self.is_processing_reals():
                self.expect_observation_transition()

    def accept_current_item(self):
        self.sources_discovered.add(self.get_current_source())
        self._process_current_item()

    def reject_current_item(self):
        self._process_current_item()

    def _process_current_item(self):
        self.get_current_workunit().process_current_item()
        self.num_processed += 1

    def next_workunit(self):
        if self.work_units.is_on_last_item():
            try:
                self._get_new_workunit()
            except NoAvailableWorkException:
                events.send(events.NO_AVAILABLE_WORK)
                return

        self.work_units.next()

    def expect_source_transition(self):
        self.expect_image_transition()

    def expect_observation_transition(self):
        self.expect_image_transition()

    def expect_image_transition(self):
        events.send(events.CHANGE_IMAGE)

    def acknowledge_image_displayed(self):
        pass

    def add_workunit(self, new_workunit):
        new_workunit.register_finished_callback(self._on_finished_workunit)
        self.work_units.append(new_workunit)
        self.download_workunit_images(new_workunit)

    def _get_new_workunit(self):
        self.add_workunit(self.workunit_provider.get_workunit())

    def is_current_source_discovered(self):
        return self.get_current_source() in self.sources_discovered

    def is_current_item_processed(self):
        return self.get_current_workunit().is_current_item_processed()

    def is_current_source_finished(self):
        return self.get_current_workunit().is_current_source_finished()

    def is_current_source_adjusted(self):
        return self.get_current_cutout().is_adjusted()

    def get_num_items_processed(self):
        return self.num_processed

    def get_current_data(self):
        return self.get_current_workunit().get_data()

    def get_current_workunit(self):
        workunit = self.work_units.get_current_item()
        if workunit is None:
            raise NoWorkUnitException()
        else:
            return workunit

    def get_writer(self):
        return self.get_current_workunit().get_writer()

    def get_current_filename(self):
        return self.get_current_workunit().get_filename()

    def get_current_source_number(self):
        return self.get_current_workunit().get_current_source_number()

    def get_current_obs_number(self):
        return self.get_current_workunit().get_current_obs_number()

    def get_obs_count(self):
        return self.get_current_workunit().get_obs_count()

    def get_current_source(self):
        return self.get_current_workunit().get_current_source()

    def get_current_reading(self):
        return self.get_current_workunit().get_current_reading()

    def get_current_astrom_header(self):
        return self.get_current_reading().get_observation_header()

    def get_current_fits_header(self):
        return self.get_current_cutout().get_fits_header()

    def get_current_exposure_number(self):
        return int(self.get_current_reading().obs.expnum)

    def get_reading_data(self):
        reading = self.get_current_reading()
        return (("X", reading.x), ("Y", reading.y), ("X_0", reading.x0),
                ("Y_0", reading.y0), ("R.A.", reading.ra),
                ("DEC", reading.dec))

    def get_header_data_list(self):
        header = self.get_current_astrom_header()
        return [(key, value) for key, value in header.iteritems()]

    def get_current_observation_date(self):
        return self.get_current_astrom_header()["MJD_OBS_CENTER"]

    def get_current_ra(self):
        try:
            return self.get_current_cutout().ra
        except ImageNotLoadedException:
            return self.get_current_reading().ra

    def get_current_dec(self):
        try:
            return self.get_current_cutout().dec
        except ImageNotLoadedException:
            return self.get_current_reading().dec

    def is_current_source_named(self):
        return self.get_current_source().has_provisional_name()

    def get_current_source_name(self):
        return self.get_current_source().get_provisional_name()

    def set_current_source_name(self, name):
        return self.get_current_source().set_provisional_name(name)

    def get_current_displayable_item(self):
        return self.image_state.get_current_displayble_item()

    def get_current_band(self):
        return self.get_current_fits_header()["FILTER"][0]

    def get_current_pixel_source_point(self):
        return self.get_current_cutout().pixel_source_point

    def get_current_source_observed_magnitude(self):
        return self.get_current_cutout().get_observed_magnitude()

    def get_current_image_FWHM(self):
        return float(self.get_current_astrom_header()["FWHM"])

    def get_current_image_maxcount(self):
        return float(self.get_current_astrom_header()["MAXCOUNT"])

    def stop_loading_images(self):
        self.image_manager.stop_downloads()

    def start_loading_images(self):
        self.download_workunit_images(self.get_current_workunit())

    def submit_download_request(self, download_request):
        self.image_state.submit_download_request(download_request)

    def refresh_vos_client(self):
        self.image_manager.refresh_vos_clients()

    def update_current_source_location(self, new_location):
        """
        Updates the location of the source in the image.

        Args:
          new_location: tuple(x, y)
            The source location using pixel coordinates from the
            displayed image (may be a cutout).
        """
        self.get_current_cutout().update_pixel_location(new_location)

    def reset_current_source_location(self):
        self.get_current_cutout().reset_source_location()

    def enable_synchronization(self):
        if self.synchronization_manager:
            self.synchronization_manager.enable_sync()
            logger.info("Synchronization enabled")

    def disable_synchronization(self):
        if self.synchronization_manager:
            self.synchronization_manager.disable_sync()
            logger.info("Synchronization disabled")

    def exit(self):
        for workunit in self.work_units:
            workunit.unlock()

        self.image_manager.stop_downloads()
        self.workunit_provider.shutdown()
        self.image_manager.wait_for_downloads_to_stop()

    def is_processing_candidates(self):
        return isinstance(self.get_current_workunit(), CandidatesWorkUnit)

    def is_processing_reals(self):
        return isinstance(self.get_current_workunit(), RealsWorkUnit)

    def get_current_cutout(self):
        return self.image_manager.get_cutout(self.get_current_reading())

    def download_workunit_images(self, workunit):
        self.image_state.download_workunit_images(workunit)

    def use_singlets(self):
        logger.info("Model set to use image singlets.")
        self.image_state = SingletState(self)
        self.image_state.enter_state()

    def use_triplets(self):
        logger.info("Model set to use image triplets.")
        self.image_state = TripletState(self)
        self.image_state.enter_state()

    def _on_finished_workunit(self, results_file_paths):
        events.send(events.FINISHED_WORKUNIT, results_file_paths)

        if self.synchronization_manager:
            for path in results_file_paths:
                self.synchronization_manager.add_syncable_file(path)


class SingletState(object):
    def __init__(self, model):
        self.model = model
        self.image_manager = model.image_manager

    def enter_state(self):
        self.image_manager.stop_triplet_downloads()
        self.download_workunit_images(
            self.model.get_current_workunit())

    def get_current_displayble_item(self):
        return self.image_manager.get_displayable_singlet(
            self.model.get_current_reading())

    def download_workunit_images(self, workunit):
        self.image_manager.download_singlets_for_workunit(workunit)

    def submit_download_request(self, download_request):
        self.image_manager.submit_singlet_download_request(download_request)


class TripletState(object):
    def __init__(self, model):
        self.model = model
        self.image_manager = model.image_manager

    def enter_state(self):
        self.image_manager.stop_singlet_downloads()
        self.download_workunit_images(
            self.model.get_current_workunit())

    def get_current_displayble_item(self):
        return self.image_manager.get_displayable_triplet(
            self.model.get_current_source())

    def download_workunit_images(self, workunit):
        self.image_manager.download_triplets_for_workunit(workunit)

    def submit_download_request(self, download_request):
        self.image_manager.submit_triplet_download_request(download_request)

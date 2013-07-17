__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui import events
from ossos.gui.workload import NoAvailableWorkException, StatefulCollection


class ImageNotLoadedException(Exception):
    """The requested image hasn't been loaded yet."""


class NoWorkUnitException(Exception):
    """No data is available at the current time."""


class UIModel(object):
    """
    Contains the data and associated operations available to the user interface.
    """

    def __init__(self, workunit_provider, progress_manager, download_manager):
        self.workunit_provider = workunit_provider
        self.progress_manager = progress_manager
        self.download_manager = download_manager

        self.work_units = StatefulCollection()

        def shift_locks(workunit1, workunit2):
            self._unlock(workunit1)
            self._lock(workunit2)

        self.work_units.register_change_item_callback(shift_locks)

        self.num_processed = 0

        # Maps each reading to its image-reading model (once downloaded)
        self._image_reading_models = {}

        self.sources_discovered = set()

    def get_working_directory(self):
        # TODO: yuck, refactor!
        return self.progress_manager.working_context.directory

    def start_work(self):
        self.next_workunit()

    def next_source(self):
        self.get_current_workunit().next_source()
        events.send(events.CHANGE_IMAGE)

    def previous_source(self):
        self.get_current_workunit().previous_source()
        events.send(events.CHANGE_IMAGE)

    def next_obs(self):
        self.get_current_workunit().next_obs()
        events.send(events.CHANGE_IMAGE)

    def previous_obs(self):
        self.get_current_workunit().previous_obs()
        events.send(events.CHANGE_IMAGE)

    def next_item(self):
        if self.get_current_workunit().is_finished():
            self.next_workunit()
        else:
            self.get_current_workunit().next_item()
            events.send(events.CHANGE_IMAGE)

    def previous_item(self):
        self.get_current_workunit().previous_vettable_item()
        events.send(events.CHANGE_IMAGE)

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

    def _get_new_workunit(self):
        new_workunit = self.workunit_provider.get_workunit()
        new_workunit.register_finished_callback(self._on_finished_workunit)
        self.work_units.append(new_workunit)
        self._download_workunit_images(new_workunit)

    def previous_workunit(self):
        self.work_units.previous()

    def is_current_source_discovered(self):
        return self.get_current_source() in self.sources_discovered

    def is_current_item_processed(self):
        return self.get_current_workunit().is_current_item_processed()

    def is_current_source_finished(self):
        return self.get_current_workunit().is_current_source_finished()

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

    def get_current_exposure_number(self):
        return int(self.get_current_reading().obs.expnum)

    def get_reading_data(self):
        reading = self.get_current_reading()
        return (("X", reading.x), ("Y", reading.y), ("X_0", reading.x0),
                ("Y_0", reading.y0), ("R.A.", reading.ra),
                ("DEC", reading.dec))

    def get_header_data_list(self):
        reading = self.get_current_reading()
        return [(key, value) for key, value in reading.obs.header.iteritems()]

    def get_current_observation_date(self):
        return self.get_current_reading().obs.header["MJD_OBS_CENTER"]

    def get_current_ra(self):
        return self.get_current_reading().ra

    def get_current_dec(self):
        return self.get_current_reading().dec

    def get_current_image(self):
        return self._get_current_image_reading().get_image()

    def get_current_hdulist(self):
        return self.get_current_image().as_hdulist()

    def get_current_band(self):
        hdu0 = self.get_current_hdulist()[0]
        return hdu0.header["FILTER"][0]

    def get_current_image_source_point(self):
        return self.get_current_image().get_pixel_coordinates(
            self.get_current_reading().source_point)

    def get_current_source_observed_magnitude(self):
        x, y = self.get_current_image_source_point()
        maxcount = self.get_current_image_maxcount()
        return self.get_current_image().get_observed_magnitude(x, y, maxcount=maxcount)

    def get_current_image_FWHM(self):
        return float(self.get_current_reading().obs.header["FWHM"])

    def get_current_image_maxcount(self):
        return float(self.get_current_reading().obs.header["MAXCOUNT"])

    def get_loaded_image_count(self):
        return len(self._image_reading_models)

    def stop_loading_images(self):
        self.download_manager.stop_download()

    def start_loading_images(self):
        self._download_workunit_images(self.get_current_workunit())

    def retry_download(self, downloadable_item):
        self.download_manager.retry_download(downloadable_item)

    def refresh_vos_client(self):
        self.download_manager.refresh_vos_client()

    def exit(self):
        try:
            self._unlock(self.get_current_workunit())
        except NoWorkUnitException:
            # Nothing to unlock
            pass

        for workunit in self.work_units:
            workunit.get_writer().close()

        self.download_manager.stop_download()
        self.download_manager.wait_for_downloads_to_stop()

    def _get_current_image_reading(self):
        try:
            return self._image_reading_models[self.get_current_reading()]
        except KeyError:
            raise ImageNotLoadedException()

    def _lock(self, workunit):
        self.progress_manager.lock(workunit.get_filename())

    def _unlock(self, workunit):
        self.progress_manager.unlock(workunit.get_filename())

    def _download_workunit_images(self, workunit):
        self.download_manager.start_downloading_workunit(
            workunit, image_loaded_callback=self._on_image_loaded)

    def _on_image_loaded(self, reading, image):
        self._image_reading_models[reading] = ImageReading(reading, image)
        events.send(events.IMG_LOADED, reading)

    def _on_finished_workunit(self, filename):
        events.send(events.FINISHED_WORKUNIT, filename)


class ImageReading(object):
    """
    Associates a particular source reading with a downloaded FITS image.
    """

    def __init__(self, reading, fits_image):
        self.reading = reading
        self.fits_image = fits_image

    def get_image(self):
        return self.fits_image


"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from pymop.gui import events
from pymop.io.workload import NoAvailableWorkException, StatefulCollection


class AbstractModel(object):
    """
    Functionality common to the models of all tasks.
    """

    def __init__(self, workunit_provider, progress_manager, download_manager):
        self.workunit_provider = workunit_provider
        self.progress_manager = progress_manager
        self.download_manager = download_manager

        self.work_units = StatefulCollection()

        def shift_locks(workunit1, workunit2):
            self._unlock(workunit1)
            self._lock(workunit2)

        self.work_units.add_callback(shift_locks)

        self.num_processed = 0
        self._num_images_loaded = 0

        self.sources_discovered = set()

        events.subscribe(events.NEW_WORK_UNIT, self._download_current_workunit_images)
        self.start_work()

    def get_current_data(self):
        return self.get_current_workunit().get_data()

    def start_work(self):
        self.next_workunit()

    def next_workunit(self):
        if not self.work_units.has_next():
            new_workunit = self.workunit_provider.get_workunit()
            new_workunit.register_finished_callback(self._on_finished_workunit)
            self.work_units.append(new_workunit)
            events.send(events.NEW_WORK_UNIT)

        self.work_units.next()

    def previous_workunit(self):
        self.work_units.previous()

    def get_current_workunit(self):
        return self.work_units.get_current_item()

    def get_current_filename(self):
        return self.get_current_workunit().get_filename()

    def get_current_source_number(self):
        return self.get_current_workunit().get_current_source_number()

    def next_source(self):
        self.get_current_workunit().next_source()

    def previous_source(self):
        self.get_current_workunit().previous_source()

    def get_current_obs_number(self):
        return self.get_current_workunit().get_current_obs_number()

    def get_obs_count(self):
        return self.get_current_workunit().get_obs_count()

    def next_obs(self):
        self.get_current_workunit().next_obs()

    def previous_obs(self):
        self.get_current_workunit().previous_obs()

    def get_current_source(self):
        return self.get_current_workunit().get_current_source()

    def get_current_reading(self):
        return self.get_current_workunit().get_current_reading()

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
        return self.get_current_reading().get_fits_image()

    def get_current_hdulist(self):
        fitsimage = self.get_current_image()

        if fitsimage is None:
            return None

        return fitsimage.as_hdulist()

    def get_current_band(self):
        hdu0 = self.get_current_hdulist()[0]
        return hdu0.header["FILTER"][0]

    def get_current_image_source_point(self):
        return self.get_current_reading().image_source_point

    def get_current_source_observed_magnitude(self):
        x, y = self.get_current_image_source_point()
        maxcount = self.get_current_image_maxcount()
        return self.get_current_image().get_observed_magnitude(x, y, maxcount=maxcount)

    def get_current_image_FWHM(self):
        return float(self.get_current_reading().obs.header["FWHM"])

    def get_current_image_maxcount(self):
        return float(self.get_current_reading().obs.header["MAXCOUNT"])

    def get_current_exposure_number(self):
        return int(self.get_current_reading().obs.expnum)

    def _download_current_workunit_images(self, event):
        self.download_manager.start_download(
            self.get_current_workunit(),
            image_loaded_callback=self._on_image_loaded)

    def stop_loading_images(self):
        self.download_manager.stop_download()

    def get_loaded_image_count(self):
        return self._num_images_loaded

    def _on_image_loaded(self, source_num, obs_num):
        self._num_images_loaded += 1
        events.send(events.IMG_LOADED, (source_num, obs_num))

    def get_num_items_processed(self):
        return self.num_processed

    def accept_current_item(self):
        self.sources_discovered.add(self.get_current_source())
        # TODO: refactor
        try:
            self._do_accept_current_item()
        except NoAvailableWorkException:
            events.send(events.NO_AVAILABLE_WORK)
            self.exit()

    def reject_current_item(self):
        # TODO refactor
        try:
            self._do_reject_current_item()
        except NoAvailableWorkException:
            events.send(events.NO_AVAILABLE_WORK)
            self.exit()

    def _do_accept_current_item(self):
        # TODO refactor
        self.get_current_workunit().accept_current_item()
        self._process_current_item()

    def _do_reject_current_item(self):
        # TODO refactor
        self.get_current_workunit().reject_current_item()
        self._process_current_item()

    def _process_current_item(self):
        # TODO refactor
        self.num_processed += 1

    def is_current_item_processed(self):
        return self.get_current_workunit().is_current_item_processed()

    def get_writer(self):
        return self.get_current_workunit().get_writer()

    def next_item(self):
        self.get_current_workunit().next_vettable_item()

    def previous_item(self):
        self.get_current_workunit().previous_vettable_item()

    def is_current_source_discovered(self):
        return self.get_current_source() in self.sources_discovered

    def exit(self):
        self._unlock(self.get_current_workunit())

    def _lock(self, workunit):
        self.progress_manager.lock(workunit.get_filename())

    def _unlock(self, workunit):
        self.progress_manager.unlock(workunit.get_filename())

    def _on_finished_workunit(self, filename):
        events.send(events.FINISHED_WORKUNIT, filename)
        self.next_workunit()


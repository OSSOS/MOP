"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import collections

# TODO: upgrade
from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

from pymop.gui import events
from pymop.io.workload import NoAvailableWorkException


class AbstractModel(object):
    """
    Functionality common to the models of all tasks.
    """

    def __init__(self, workload_manager, download_manager):
        self.workload_manager = workload_manager
        self.download_manager = download_manager

        self._num_images_loaded = 0

        pub.subscribe(self._download_current_workunit_images, events.MSG_NEW_WORK_UNIT)
        self.workload_manager.start_work()

    def get_current_filename(self):
        return self.workload_manager.get_current_filename()

    def get_current_source_number(self):
        return self.workload_manager.get_current_source_number()

    def next_source(self):
        self.workload_manager.next_source()

    def previous_source(self):
        self.workload_manager.previous_source()

    def get_current_obs_number(self):
        return self.workload_manager.get_current_obs_number()

    def get_obs_count(self):
        return self.workload_manager.get_obs_count()

    def next_obs(self):
        self.workload_manager.next_obs()

    def previous_obs(self):
        self.workload_manager.previous_obs()

    def get_current_source(self):
        return self.workload_manager.get_current_source()

    def get_current_reading(self):
        return self.workload_manager.get_current_reading()

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
            self.workload_manager.get_current_workunit(),
            image_loaded_callback=self._on_image_loaded)

    def stop_loading_images(self):
        self.download_manager.stop_download()

    def get_loaded_image_count(self):
        return self._num_images_loaded

    def _on_image_loaded(self, source_num, obs_num):
        self._num_images_loaded += 1
        pub.sendMessage(events.MSG_IMG_LOADED, (source_num, obs_num))

    def get_num_items_processed(self):
        return self.workload_manager.get_num_items_processed()

    def accept_current_item(self):
        # TODO: refactor
        try:
            self.workload_manager.accept_current_item()
        except NoAvailableWorkException:
            pub.sendMessage(events.MSG_ALL_ITEMS_PROC)
            self.exit()
        finally:
            self._on_accept()

    def _on_accept(self):
        """Hook you can override to do extra processing when accepting an item."""
        pass

    def reject_current_item(self):
        # TODO refactor
        try:
            self.workload_manager.reject_current_item()
        except NoAvailableWorkException:
            pub.sendMessage(events.MSG_ALL_ITEMS_PROC)
            self.exit()

    def is_current_item_processed(self):
        return self.workload_manager.is_current_item_processed()

    def get_writer(self):
        return self.workload_manager.get_writer()

    def next_item(self):
        self.workload_manager.next_item()

    def previous_item(self):
        self.workload_manager.previous_item()

    def exit(self):
        self.workload_manager.exit()


class ProcessRealsModel(AbstractModel):
    """
    Manages the application state for the process reals task.
    TODO: refactor
    """

    def __init__(self, workload_manager, download_manager):
        super(ProcessRealsModel, self).__init__(
            workload_manager, download_manager)

        self._source_discovery_asterisk = collections.defaultdict(lambda: False)

    def _on_accept(self):
        self._source_discovery_asterisk[self.get_current_source()] = True

    def is_current_source_discovered(self):
        return self._source_discovery_asterisk[self.get_current_source()]


class ProcessCandidatesModel(AbstractModel):
    """TODO: refactor"""
    def __init__(self, workload_manager, download_manager):
        super(ProcessCandidatesModel, self).__init__(
            workload_manager, download_manager)


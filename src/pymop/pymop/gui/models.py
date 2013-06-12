"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from wx.lib.pubsub import Publisher as pub

# Pub/Sub ids
MSG_ROOT = ("astrodataroot", )

MSG_NAV = MSG_ROOT + ("nav", )
MSG_NAV_SRC = MSG_NAV + ("src", )
MSG_NAV_OBS = MSG_NAV + ("obs", )

MSG_NEXT_SRC = MSG_NAV_SRC + ("next", )
MSG_PREV_SRC = MSG_NAV_SRC + ("prev", )
MSG_NEXT_OBS = MSG_NAV_OBS + ("next", )
MSG_PREV_OBS = MSG_NAV_OBS + ("prev", )

MSG_IMG_LOADED = MSG_ROOT + ("imgload", )

MSG_ALL_ITEMS_PROC = MSG_ROOT + ("allproc", )


class VettableItem(object):
    ACCEPTED = "accepted"
    REJECTED = "rejected"
    UNPROCESSED = "unprocessed"

    def __init__(self, item):
        self.item = item
        self._status = VettableItem.UNPROCESSED

    def is_processed(self):
        return self._status != VettableItem.UNPROCESSED

    def is_accepted(self):
        return self._status == VettableItem.ACCEPTED

    def is_rejected(self):
        return self._status == VettableItem.REJECTED

    def accept(self):
        self._status = VettableItem.ACCEPTED

    def reject(self):
        self._status = VettableItem.REJECTED

    def get_status(self):
        return self._status


class ProcessRealsModel(object):
    """
    Main model for storing and accessing astronomical data in the
    application.
    """

    def __init__(self, astrom_data, download_manager):
        self.astrom_data = astrom_data
        self.download_manager = download_manager

        self._current_src_number = 0
        self._current_obs_number = 0

        self._num_images_loaded = 0

        self._vettable_items = {}
        for source in self.astrom_data.sources:
            for reading in source:
                self._vettable_items[reading] = VettableItem(reading)

        self._source_discovery_asterisk = [False] * self.get_source_count()

    def next_item(self):
        """Move to the next item to process."""
        if self.get_current_obs_number() == self.get_obs_count() - 1:
            self.next_source()
            self._current_obs_number = 0
        else:
            self.next_obs()

    def get_current_item(self):
        reading = self.astrom_data.sources[self.get_current_source_number()].get_reading(self.get_current_obs_number())
        return self._vettable_items[reading]

    def get_current_source_number(self):
        return self._current_src_number

    def get_source_count(self):
        return len(self.astrom_data.sources)

    def next_source(self):
        self._current_src_number = (self._current_src_number + 1) % self.get_source_count()
        pub.sendMessage(MSG_NEXT_SRC, data=self._current_src_number)

    def previous_source(self):
        self._current_src_number = (self._current_src_number - 1) % self.get_source_count()
        pub.sendMessage(MSG_PREV_SRC, data=self._current_src_number)

    def get_current_obs_number(self):
        return self._current_obs_number

    def get_obs_count(self):
        return self._get_current_source().num_readings()

    def next_obs(self):
        self._current_obs_number = (self._current_obs_number + 1) % self.get_obs_count()
        pub.sendMessage(MSG_NEXT_OBS, data=self._current_obs_number)

    def previous_obs(self):
        self._current_obs_number = (self._current_obs_number - 1) % self.get_obs_count()
        pub.sendMessage(MSG_PREV_OBS, data=self._current_obs_number)

    def _get_current_source(self):
        return self.astrom_data.sources[self._current_src_number]

    def _get_current_reading(self):
        return self._get_current_source().get_reading(self._current_obs_number)

    def get_reading_data(self):
        reading = self._get_current_reading()
        return (("X", reading.x), ("Y", reading.y), ("X_0", reading.x0),
                ("Y_0", reading.y0), ("R.A.", reading.ra),
                ("DEC", reading.dec))

    def get_header_data_list(self):
        reading = self._get_current_reading()
        return [(key, value) for key, value in reading.obs.header.iteritems()]

    def get_current_observation_date(self):
        return self._get_current_reading().obs.header["MJD_OBS_CENTER"]

    def get_current_ra(self):
        return self._get_current_reading().ra

    def get_current_dec(self):
        return self._get_current_reading().dec

    def get_current_image(self):
        return self._get_current_reading().get_fits_image()

    def get_current_hdulist(self):
        fitsimage = self.get_current_image()

        if fitsimage is None:
            return None

        return fitsimage.as_hdulist()

    def get_current_band(self):
        hdu0 = self.get_current_hdulist()[0]
        return hdu0.header["FILTER"][0]

    def get_current_image_source_point(self):
        return self._get_current_reading().image_source_point

    def get_current_source_observed_magnitude(self):
        x, y = self.get_current_image_source_point()
        maxcount = self.get_current_image_maxcount()
        return self.get_current_image().get_observed_magnitude(x, y, maxcount=maxcount)

    def get_current_image_FWHM(self):
        return float(self._get_current_reading().obs.header["FWHM"])

    def get_current_image_maxcount(self):
        return float(self._get_current_reading().obs.header["MAXCOUNT"])

    def get_current_exposure_number(self):
        return int(self._get_current_reading().obs.expnum)

    def start_loading_images(self):
        self.download_manager.start_download(
            self.astrom_data, image_loaded_callback=self._on_image_loaded)

    def stop_loading_images(self):
        self.download_manager.stop_download()

    def get_loaded_image_count(self):
        return self._num_images_loaded

    def get_item_count(self):
        return self.astrom_data.get_reading_count()

    def get_total_image_count(self):
        return self.get_item_count()

    def _on_image_loaded(self, source_num, obs_num):
        self._num_images_loaded += 1
        pub.sendMessage(MSG_IMG_LOADED, (source_num, obs_num))

    def accept_current_item(self):
        self.get_current_item().accept()
        self._source_discovery_asterisk[self.get_current_source_number()] = True
        self._check_if_finished()

    def reject_current_item(self):
        self.get_current_item().reject()
        self._check_if_finished()

    def _check_if_finished(self):
        if self.get_num_items_processed() == self.get_item_count():
            pub.sendMessage(MSG_ALL_ITEMS_PROC)

    def get_num_items_processed(self):
        processed = [item.is_processed() for item in self._vettable_items.values()]
        return processed.count(True)

    def is_item_processed(self, reading):
        return self._vettable_items[reading].is_processed()

    def get_item_status(self, reading):
        return self._vettable_items[reading].get_status()

    def is_current_source_discovered(self):
        return self._source_discovery_asterisk[self.get_current_source_number()]


"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import os

from wx.lib.pubsub import Publisher as pub

from pymop.io.mpc import MPCWriter
from pymop.io.astrom import AstromWriter

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


class AbstractModel(object):
    """
    Functionality common to the models of all tasks.
    """

    def __init__(self, workload, download_manager):
        self.workload = workload
        self.download_manager = download_manager

        self._current_astrom_data_number = 0

        # These indices are within the current astrom data
        self._current_src_number = 0
        self._current_obs_number = 0

        self._num_images_loaded = 0

        self._vettable_items = self._create_vettable_items()

    def _get_current_astrom_data(self):
        return self.workload.get_astrom_data(self._current_astrom_data_number)

    def _create_vettable_items(self):
        raise NotImplementedError()

    def get_current_source_number(self):
        already_processed = 0
        for index in range(self._current_astrom_data_number):
            processed_data = self.workload.get_astrom_data(index)
            already_processed += processed_data.get_source_count()

        return already_processed + self._current_src_number

    def get_source_count(self):
        return self.workload.get_source_count()

    def next_source(self):
        if self._current_src_number + 1 == self._get_current_astrom_data().get_source_count():
            self._current_astrom_data_number = (self._current_astrom_data_number + 1) % self.workload.get_load_length()
            self._current_src_number = 0
        else:
            self._current_src_number += 1

        self._current_obs_number = 0
        pub.sendMessage(MSG_NEXT_SRC, data=self._current_src_number)

    def previous_source(self):
        if self._current_src_number == 0:
            self._current_astrom_data_number = (self._current_astrom_data_number - 1) % self.workload.get_load_length()
            self._current_src_number = self._get_current_astrom_data().get_source_count() - 1
        else:
            self._current_src_number -= 1

        pub.sendMessage(MSG_PREV_SRC, data=self._current_src_number)

    def get_current_obs_number(self):
        return self._current_obs_number

    def get_obs_count(self):
        return self.get_current_source().num_readings()

    def next_obs(self):
        self._current_obs_number = (self._current_obs_number + 1) % self.get_obs_count()
        pub.sendMessage(MSG_NEXT_OBS, data=self._current_obs_number)

    def previous_obs(self):
        self._current_obs_number = (self._current_obs_number - 1) % self.get_obs_count()
        pub.sendMessage(MSG_PREV_OBS, data=self._current_obs_number)

    def get_current_source(self):
        return self._get_current_astrom_data().sources[self._current_src_number]

    def _get_current_reading(self):
        return self.get_current_source().get_reading(self._current_obs_number)

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
            self.workload, image_loaded_callback=self._on_image_loaded)

    def stop_loading_images(self):
        self.download_manager.stop_download()

    def get_loaded_image_count(self):
        return self._num_images_loaded

    def get_item_count(self):
        return len(self._vettable_items)

    def get_total_image_count(self):
        return self.workload.get_reading_count()

    def _on_image_loaded(self, source_num, obs_num):
        self._num_images_loaded += 1
        pub.sendMessage(MSG_IMG_LOADED, (source_num, obs_num))

    def _check_if_finished(self):
        if self.get_num_items_processed() == self.get_item_count():
            pub.sendMessage(MSG_ALL_ITEMS_PROC)

    def get_num_items_processed(self):
        processed = [item.is_processed() for item in self._vettable_items.values()]
        return processed.count(True)

    def is_item_processed(self, item):
        return self._vettable_items[item].is_processed()

    def get_item_status(self, item):
        return self._vettable_items[item].get_status()

    def next_item(self):
        raise NotImplementedError()

    def accept_current_item(self):
        self.get_current_item().accept()
        self._on_accept()
        self._check_if_finished()

    def _on_accept(self):
        """Hook you can override to do extra processing when accepting an item."""
        pass

    def reject_current_item(self):
        self.get_current_item().reject()
        self._check_if_finished()

    def get_current_item(self):
        raise NotImplementedError()

    def get_writer(self):
        raise NotImplementedError()

    def exit(self):
        pass


class ProcessRealsModel(AbstractModel):
    """
    Manages the application state for the process reals task.
    """

    def __init__(self, workload, download_manager):
        super(ProcessRealsModel, self).__init__(workload, download_manager)

        self._create_vettable_items()

        self._source_discovery_asterisk = [False] * self.get_source_count()

        output_filename = os.path.join(self.workload.get_working_directory(), "reals.mpc")
        self.output_file = open(output_filename, "wb")
        self.writer = MPCWriter(self.output_file)

    def _create_vettable_items(self):
        vettable_items = {}
        for source in self.workload.get_sources():
            for reading in source:
                vettable_items[reading] = VettableItem(reading)

        return vettable_items

    def exit(self):
        self.output_file.close()

    def next_item(self):
        """Move to the next item to process."""
        if self.get_current_obs_number() == self.get_obs_count() - 1:
            self.next_source()
            self._current_obs_number = 0
        else:
            self.next_obs()

    def get_current_item(self):
        return self._vettable_items[self._get_current_reading()]

    def _on_accept(self):
        self._source_discovery_asterisk[self.get_current_source_number()] = True

    def is_current_source_discovered(self):
        return self._source_discovery_asterisk[self.get_current_source_number()]

    def get_writer(self):
        return self.writer


class ProcessCandidatesModel(AbstractModel):
    def __init__(self, workload, download_manager):
        super(ProcessCandidatesModel, self).__init__(workload, download_manager)

        self.outputfiles = []
        self.writers = []
        for input_filename, astrom_data in self.workload:
            output_filename = os.path.join(self.workload.get_working_directory(),
                                           input_filename.replace(".cands.astrom", ".reals.astrom"))
            output_filehandle = open(output_filename, "wb")
            self.outputfiles.append(output_filehandle)

            writer = AstromWriter(output_filehandle)
            writer.write_headers(astrom_data.observations, astrom_data.sys_header)
            self.writers.append(writer)

    def _create_vettable_items(self):
        vettable_items = {}
        for source in self.workload.get_sources():
            vettable_items[source] = VettableItem(source)

        return vettable_items

    def exit(self):
        for outputfile in self.outputfiles:
            outputfile.close()

    def next_item(self):
        self.next_source()

    def get_current_item(self):
        return self._vettable_items[self.get_current_source()]

    def get_writer(self):
        return self.writers[self._current_astrom_data_number]

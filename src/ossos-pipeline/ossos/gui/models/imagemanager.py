__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui import events, logger
from ossos.downloads.async import DownloadRequest
from ossos.downloads.cutouts.focus import (SingletFocusCalculator,
                                           TripletFocusCalculator)
from ossos.downloads.cutouts.triplets import CutoutGrid
from ossos.fitsviewer.displayable import (DisplayableImageSinglet,
                                          DisplayableImageTriplet)
from ossos.gui.models.exceptions import ImageNotLoadedException


class ImageManager(object):
    """
    TODO: refactor duplication.
    """

    def __init__(self, singlet_download_manager, triplet_download_manager):
        self._singlet_download_manager = singlet_download_manager
        self._triplet_download_manager = triplet_download_manager

        self._cutouts = {}
        self._displayable_singlets = {}
        self._displayable_triplets = {}

        self._workunits_downloaded_for_singlets = set()
        self._workunits_downloaded_for_triplets = set()

    def submit_singlet_download_request(self, download_request):
        self._singlet_download_manager.submit_request(download_request)

    def download_singlets_for_workunit(self, workunit):
        if workunit in self._workunits_downloaded_for_singlets:
            return

        logger.debug("Starting to download singlets for workunit: %s" %
                     workunit.get_filename())

        self._workunits_downloaded_for_singlets.add(workunit)

        needs_apcor = workunit.is_apcor_needed()
        for source in workunit.get_unprocessed_sources():
            self.download_singlets_for_source(source, needs_apcor=needs_apcor)

    def download_singlets_for_source(self, source, needs_apcor=False):
        focus_calculator = SingletFocusCalculator(source)

        for reading in source.get_readings():
            focus = focus_calculator.calculate_focus(reading)
            self._singlet_download_manager.submit_request(
                DownloadRequest(reading,
                                needs_apcor=needs_apcor,
                                focus=focus,
                                callback=self.on_singlet_image_loaded)
            )

    def get_displayable_singlet(self, reading):
        try:
            return self._displayable_singlets[reading]
        except KeyError:
            raise ImageNotLoadedException()

    def get_cutout(self, reading):
        try:
            return self._cutouts[reading]
        except KeyError:
            raise ImageNotLoadedException()

    def download_triplets_for_workunit(self, workunit):
        if workunit in self._workunits_downloaded_for_triplets:
            return

        logger.debug("Starting to download triplets for workunit: %s" %
                     workunit.get_filename())

        self._workunits_downloaded_for_triplets.add(workunit)

        for source in workunit.get_unprocessed_sources():
            self.download_triplets_for_source(source)

    def download_triplets_for_source(self, source, needs_apcor=False):
        focus_calculator = TripletFocusCalculator(source)
        grid = CutoutGrid(source)

        def create_callback(frame_index, time_index):
            def callback(cutout):
                grid.add_cutout(cutout, frame_index, time_index)

                if grid.is_filled():
                    self._displayable_triplets[grid.source] = DisplayableImageTriplet(grid)

            return callback

        for time_index, reading in enumerate(source.get_readings()):
            for frame_index in range(source.num_readings()):
                callback = create_callback(frame_index, time_index)

                focus = focus_calculator.calculate_focus(reading, frame_index)
                self._triplet_download_manager.submit_request(
                    DownloadRequest(reading,
                                    needs_apcor=needs_apcor,
                                    focus=focus,
                                    callback=callback)
                )

    def get_displayable_triplet(self, source):
        try:
            return self._displayable_triplets[source]
        except KeyError:
            raise ImageNotLoadedException()

    def stop_downloads(self):
        self.stop_singlet_downloads()
        self.stop_triplet_downloads()

    def stop_singlet_downloads(self):
        self._singlet_download_manager.stop_download()

    def stop_triplet_downloads(self):
        self._triplet_download_manager.stop_download()

    def wait_for_downloads_to_stop(self):
        self._singlet_download_manager.wait_for_downloads_to_stop()
        self._triplet_download_manager.wait_for_downloads_to_stop()

    def refresh_vos_clients(self):
        self._singlet_download_manager.refresh_vos_client()
        self._triplet_download_manager.refresh_vos_client()

    def on_singlet_image_loaded(self, cutout):
        reading = cutout.reading
        self._cutouts[reading] = cutout
        self._displayable_singlets[reading] = DisplayableImageSinglet(
            cutout.hdulist)
        events.send(events.IMG_LOADED, reading)

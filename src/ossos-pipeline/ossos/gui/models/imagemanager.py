__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.downloads.focus import SingletFocalPointCalculator
from ossos.downloads.requests import DownloadRequest
from ossos.fitsviewer.displayable import DisplayableImageSinglet
from ossos.gui import events, logger
from ossos.gui.models.exceptions import ImageNotLoadedException


class ImageManager(object):
    def __init__(self, singlet_download_manager):
        self._singlet_download_manager = singlet_download_manager
        self._singlet_focal_point_calculator = SingletFocalPointCalculator()

        self._displayable_singlets = {}
        self._snapshots = {}

    def submit_singlet_download_request(self, download_request):
        self._singlet_download_manager.submit_request(download_request)

    def download_singlets_for_workunit(self, workunit):
        logger.debug("Starting to download workunit: %s" %
                     workunit.get_filename())

        needs_apcor = workunit.is_apcor_needed()
        focal_points = []
        for source in workunit.get_unprocessed_sources():
            focal_points.extend(
                self._singlet_focal_point_calculator.calculate_focal_points(source))

        for focal_point in focal_points:
            self._singlet_download_manager.submit_request(
                DownloadRequest(focal_point.reading,
                                needs_apcor=needs_apcor,
                                focal_point=focal_point.point,
                                callback=self._on_singlet_image_loaded))

    def get_displayable_singlet(self, reading):
        try:
            return self._displayable_singlets[reading]
        except KeyError:
            raise ImageNotLoadedException()

    def get_snapshot(self, reading):
        try:
            return self._snapshots[reading]
        except KeyError:
            raise ImageNotLoadedException()

    def stop_downloads(self):
        self._singlet_download_manager.stop_download()

    def wait_for_downloads_to_stop(self):
        self._singlet_download_manager.wait_for_downloads_to_stop()

    def refresh_vos_clients(self):
        self._singlet_download_manager.refresh_vos_client()

    def _on_singlet_image_loaded(self, snapshot):
        reading = snapshot.reading
        self._snapshots[reading] = snapshot
        self._displayable_singlets[reading] = DisplayableImageSinglet(
            snapshot.hdulist)
        events.send(events.IMG_LOADED, reading)


__author__ = "David Rusk <drusk@uvic.ca>"

import errno
import os

import requests

from ossos.gui import logger


class DownloadErrorHandler(object):
    def __init__(self, app):
        self.app = app

        self._failed_downloads = []

    def handle_error(self, error, downloadable_item):
        """
        Checks what error occured and looks for an appropriate solution.

        Args:
          error: Exception
            The error that has occured.
          downloadable_item: ossos.downloads.DownloadableItem
            The item that was being downloaded when the error occurred.
        """
        if not hasattr(error, "errno"):
            logger.critical("Unresolvable download error: %s" % str(error))
            raise error

        if error.errno == errno.EACCES:
            self.handle_certificate_problem(str(error))
        elif error.errno == errno.ECONNREFUSED:
            self.handle_connection_refused(str(error), downloadable_item)
        else:
            logger.critical("Unresolvable download error: %s" % str(error))
            raise error

    def handle_certificate_problem(self, error_message):
        logger.warning("Problem with CADC certificate")

        self.app.get_model().stop_loading_images()

        view = self.app.get_view()
        view.hide_image_loading_dialog()
        view.show_certificate_dialog(self, error_message)

    def refresh_certificate(self, username, password):
        download_certificate(username, password)

        logger.info("Downloaded new CADC certificate")

        model = self.app.get_model()
        model.refresh_vos_client()

        self.app.get_view().show_image_loading_dialog()
        model.start_loading_images()

    def handle_connection_refused(self, error_message, downloadable_item):
        logger.warning("Connection refused when downloading")

        self._failed_downloads.append(downloadable_item)
        self.app.get_view().show_retry_download_dialog(self, error_message)

    def retry_downloads(self):
        model = self.app.get_model()
        for downloadable_item in self._failed_downloads:
            model.retry_download(downloadable_item)


def download_certificate(username, password):
    url = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cred/proxyCert?daysValid=7"
    response = requests.get(url, auth=(username, password))

    certfile = os.path.join(os.getenv("HOME"), ".ssl/cadcproxy.pem")
    with open(certfile, "wb") as filehandle:
        filehandle.write(response.content)

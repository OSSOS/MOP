import traceback

__author__ = "David Rusk <drusk@uvic.ca>"

import errno
import os

import requests

from ossos.gui import logger


class DownloadErrorHandler(object):
    def __init__(self, app):
        self.app = app

        self._failed_downloads = []

    def handle_error(self, error, download_request):
        """
        Checks what error occured and looks for an appropriate solution.

        Args:
          error: Exception
            The error that has occured.
          download_request:
            The request which resulted in the error.
        """
        if hasattr(error, "errno") and error.errno == errno.EACCES:
            self.handle_certificate_problem(str(error))
        else:
            self.handle_general_download_error(str(error), download_request)

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

    def handle_general_download_error(self, error_message, download_request):
        logger.warning("A problem occurred while downloading: %s" % error_message)
        logger.error("-" * 60)
        logger.error(traceback.format_exc())
        logger.error("-" * 60)

        self._failed_downloads.append(download_request)
        self.app.get_view().show_retry_download_dialog(self, error_message)

    def retry_downloads(self):
        model = self.app.get_model()
        for download_request in self._failed_downloads:
            model.submit_download_request(download_request)


def download_certificate(username, password):
    url = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cred/proxyCert?daysValid=7"
    response = requests.get(url, auth=(username, password))

    certfile = os.path.join(os.getenv("HOME"), ".ssl/cadcproxy.pem")
    with open(certfile, "wb") as filehandle:
        filehandle.write(response.content)

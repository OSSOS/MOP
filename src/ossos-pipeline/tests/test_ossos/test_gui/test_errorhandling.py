__author__ = "David Rusk <drusk@uvic.ca>"

import errno
import unittest

from mock import Mock

from ossos.gui.app import ValidationApplication
from ossos.gui.errorhandling import VOSpaceErrorHandler
from ossos.gui.views import ApplicationView
from ossos.gui.downloads import DownloadableItem


class VOSpaceErrorHandlerTest(unittest.TestCase):
    def setUp(self):
        app = Mock(spec=ValidationApplication)
        view = Mock(spec=ApplicationView)
        app.get_view.return_value = view

        self.downloadable_item = Mock(spec=DownloadableItem)

        self.error_handler = VOSpaceErrorHandler(app)
        self.view = view

    def test_handle_certificate_problem(self):
        message = "Your certificate is expired."
        error = OSError(message)
        error.errno = errno.EACCES

        self.error_handler.handle_error(error, self.downloadable_item)

        self.view.show_certificate_dialog.assert_called_once_with(
            self.error_handler, message)

    def test_handle_connection_timeout(self):
        message = "Connection timed out."
        error = IOError(message)
        error.errno = errno.ECONNREFUSED

        self.error_handler.handle_error(error, self.downloadable_item)

        self.view.show_retry_download_dialog.assert_called_once_with(
            self.error_handler, message)


if __name__ == '__main__':
    unittest.main()

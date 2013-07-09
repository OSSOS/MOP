__author__ = "David Rusk <drusk@uvic.ca>"

import errno
import unittest

from mock import Mock

from ossos.gui.app import ValidationApplication
from ossos.gui.errorhandling import VOSpaceErrorHandler
from ossos.gui.views import ApplicationView


class VOSpaceErrorHandlerTest(unittest.TestCase):
    def test_handle_certificate_problem(self):
        app = Mock(spec=ValidationApplication)
        view = Mock(spec=ApplicationView)
        app.get_view.return_value = view

        error_handler = VOSpaceErrorHandler(app)

        message = "Your certificate is expired."
        error = OSError(message)
        error.errno = errno.EACCES

        error_handler.handle_error(error)

        view.show_certificate_dialog.assert_called_once_with(error_handler,
                                                             message)


if __name__ == '__main__':
    unittest.main()

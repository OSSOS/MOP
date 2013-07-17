__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import patch

from tests.base_tests import FileReadingTestCase
from ossos.gui import tasks
from ossos.gui.app import ValidationApplication


class ValidationApplicationTest(FileReadingTestCase):
    @patch("ossos.gui.controllers.ApplicationView")
    def test_empty_workload_triggers_dialog(self, mock_class):
        # XXX have to patch where the class is invoked, and the controller
        # currently creates the view.  TODO Refactor.

        working_directory = self.get_abs_path("data/empty")
        app = ValidationApplication(tasks.REALS_TASK, working_directory)

        assert_that(app.get_view().show_empty_workload_dialog.call_count,
                    equal_to(1))


if __name__ == '__main__':
    unittest.main()

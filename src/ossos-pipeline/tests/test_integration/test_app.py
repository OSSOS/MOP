__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import patch

from tests.base_tests import FileReadingTestCase
from ossos.gui.app import ProcessRealsApplication


class ValidationApplicationTest(FileReadingTestCase):
    @patch("ossos.gui.views.dialogs.show_empty_workload_dialog")
    def test_empty_workload_triggers_dialog(self, mock_dialog):
        working_directory = self.get_abs_path("data/empty")
        output_directory = working_directory
        ProcessRealsApplication(working_directory, output_directory)

        assert_that(mock_dialog.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

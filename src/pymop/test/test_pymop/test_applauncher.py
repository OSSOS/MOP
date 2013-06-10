__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import tempfile

from mock import patch, call
from hamcrest import assert_that, contains

from test.base_tests import FileReadingTestCase
from pymop.applauncher import AstromFileApplicationLauncher


class AstromFileApplicationLauncherTest(FileReadingTestCase):
    def setUp(self):
        self.outputfile = tempfile.NamedTemporaryFile(mode="wb")

    def tearDown(self):
        self.outputfile.close()

    @patch("wx.CallAfter")
    @patch("pymop.io.imgaccess.AsynchronousImageDownloadManager")
    def test_run_startup(self, mock_image_loader, mock_wxCallAfter):
        launcher = AstromFileApplicationLauncher()
        appcontrol = launcher.run(self.get_abs_path("data/1584431p15.measure3.cands.astrom"),
                                  self.outputfile.name, False, unittest=True)

        # Check some things that should happen on startup
        assert_that(mock_wxCallAfter.call_args_list,
                    contains(call(appcontrol.get_view().as_widget().show_image_loading_dialog),
                             call(appcontrol.get_model().start_loading_images)))


if __name__ == '__main__':
    unittest.main()

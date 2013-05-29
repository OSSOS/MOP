__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from hamcrest import assert_that, equal_to

from pymop.gui.view.core.statusview import AppStatusBar


class AppStatusBarTest(unittest.TestCase):
    def setUp(self):
        self.app = wx.App()
        self.rootframe = wx.Frame(None)
        self.undertest = AppStatusBar(self.rootframe)

    def test_set_get_statuses(self):
        assert_that(self.undertest.get_source_status(), equal_to(AppStatusBar.LOADING_MSG))
        assert_that(self.undertest.get_loading_status(), equal_to(AppStatusBar.LOADING_MSG))

        self.undertest.set_source_status(1, 3)
        self.undertest.set_loading_status(48, 50)

        assert_that(self.undertest.get_source_status(), equal_to("Source 1 of 3"))
        assert_that(self.undertest.get_loading_status(), equal_to("Loaded 48 of 50 images"))


if __name__ == '__main__':
    unittest.main()

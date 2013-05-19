__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from mopgui.view.acceptsourceview import AcceptSourceDialog

TEST_PROVISIONAL_NAME = "provisional-name-1"


class AcceptSourceDialogTest(WxWidgetTestCase):
    def setUp(self):
        self.app = wx.App()
        self.rootframe = wx.Frame(None)

    def tearDown(self):
        self.rootframe.Destroy()

    def test_create_components(self):
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME)

        component_labels = [AcceptSourceDialog.MINOR_PLANET_NUMBER, AcceptSourceDialog.PROVISIONAL_NAME,
                            AcceptSourceDialog.DISCOVERY_ASTERISK]

        for label in component_labels:
            self.assert_has_child_with_label(undertest, label)

    def test_preset_values(self):
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME)

        assert_that(undertest.GetTitle(), equal_to(AcceptSourceDialog.TITLE))
        self.assert_has_child_with_label(undertest, TEST_PROVISIONAL_NAME)


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
import time

import wx
from hamcrest import assert_that, greater_than, equal_to
from mock import Mock, patch

from test.base_tests import WxWidgetTestCase
from mopgui.view.dialogs import WaitingGaugeDialog


class WaitingGaugeDialogTest(WxWidgetTestCase):
    def setUp(self):
        self.app = wx.App()
        self.rootframe = wx.Frame(None)

    def tearDown(self):
        self.rootframe.Destroy()

    @patch.object(WaitingGaugeDialog, "_on_tick")
    def test_dialog(self, on_tick_mock):
        tick_time = 10
        wait_msg = "Please wait..."
        undertest = WaitingGaugeDialog(self.rootframe, wait_msg,
                                       pulse_period_ms=tick_time)

        assert_that(undertest.msg.GetLabelText(), equal_to(wait_msg))

        # TODO: test ticker? doesn't seem to fire in test case..
        # Wait for a bit more than a tick period then check ticker activated
        # time.sleep((float(tick_time) / 1000) * 2.25)
        # assert_that(on_tick_mock.call_count, greater_than(0))


if __name__ == '__main__':
    unittest.main()

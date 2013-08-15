__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to
from mock import patch

from tests.base_tests import WxWidgetTestCase
from ossos.gui.views.loading import WaitingGaugeDialog


class WaitingGaugeDialogTest(WxWidgetTestCase):
    def setUp(self):
        super(WaitingGaugeDialogTest, self).setUp()

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

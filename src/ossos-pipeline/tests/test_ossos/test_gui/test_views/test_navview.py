__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock
from hamcrest import assert_that, equal_to, not_none

from tests.base_tests import WxWidgetTestCase
from ossos.gui.views.navigation import NavPanel


class NavigationPanelTest(WxWidgetTestCase):
    def setUp(self):
        super(NavigationPanelTest, self).setUp()

        self.controller = Mock()
        self.undertest = NavPanel(self.rootframe, self.controller)

    def test_press_next_source_button(self):
        next_button = self.get_child_by_label(self.undertest, NavPanel.NEXT_LABEL)
        assert_that(next_button, not_none())

        self.fire_button_click_event(next_button)
        assert_that(self.controller.on_next_obs.call_count, equal_to(1))

    def test_press_next_source_button(self):
        prev_button = self.get_child_by_label(self.undertest, NavPanel.PREV_LABEL)
        assert_that(prev_button, not_none())

        self.fire_button_click_event(prev_button)
        assert_that(self.controller.on_previous_obs.call_count, equal_to(1))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from mock import Mock
from hamcrest import assert_that, equal_to

from tests.base_tests import WxWidgetTestCase
from ossos.gui.views.validation import KeyboardCompleteComboBox


class KeyboardCompleteComboBoxTest(WxWidgetTestCase):
    def setUp(self):
        super(KeyboardCompleteComboBoxTest, self).setUp()

        self.choice_default = ""
        self.choice_a = "A choice a"
        self.choice_b = "B choice b"
        self.choice_c = "C choice c"

        choices = [self.choice_default, self.choice_a, self.choice_b,
                   self.choice_c]

        self.undertest = KeyboardCompleteComboBox(self.rootframe, choices=choices)

        assert_that(self.undertest.GetValue(), equal_to(self.choice_default))

    def fire_mock_key_event(self, keycode):
        self.event = Mock(spec=wx.KeyEvent)
        self.event.GetKeyCode.return_value = keycode
        self.undertest._on_char(self.event)

    def get_selection(self):
        return self.undertest.GetStringSelection()

    def test_keyboard_completion(self):
        self.fire_mock_key_event(ord("B"))
        assert_that(self.get_selection(), equal_to(self.choice_b))

    def test_press_enter(self):
        self.fire_mock_key_event(wx.WXK_RETURN)
        self.event.Skip.assert_called_once_with()
        assert_that(self.get_selection(), equal_to(self.choice_default))
        self.fire_mock_key_event(ord("C"))
        assert_that(self.get_selection(), equal_to(self.choice_c))
        self.fire_mock_key_event(wx.WXK_RETURN)
        assert_that(self.get_selection(), equal_to(self.choice_c))
        self.event.Skip.assert_called_once_with()

    def test_invalid_character(self):
        self.fire_mock_key_event(wx.WXK_UP)
        assert_that(self.get_selection(), equal_to(self.choice_default))


if __name__ == '__main__':
    unittest.main()

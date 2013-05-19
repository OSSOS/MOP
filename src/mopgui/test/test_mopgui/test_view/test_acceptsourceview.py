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
                            AcceptSourceDialog.DISCOVERY_ASTERISK, AcceptSourceDialog.NOTE1,
                            AcceptSourceDialog.NOTE2]

        for label in component_labels:
            self.assert_has_child_with_label(undertest, label)

    def test_required_preset_values(self):
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME)

        assert_that(undertest.GetTitle(), equal_to(AcceptSourceDialog.TITLE))
        self.assert_has_child_with_label(undertest, TEST_PROVISIONAL_NAME)

        note1_combobox = self.get_child_by_label(undertest, AcceptSourceDialog.NOTE1)
        assert_that(note1_combobox)

    def test_note_comboboxes_populated(self):
        note1_choices = ["n1a", "n1b"]
        note2_choices = ["n2a", "n2b", "n2c"]
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME,
                                       note1_choices=note1_choices,
                                       note2_choices=note2_choices)

        note1_combobox = self.get_child_by_name(undertest, AcceptSourceDialog.NOTE1)
        assert_that(note1_combobox.GetValue(), equal_to(""))
        assert_that(not note1_combobox.IsEditable())
        assert_that(note1_combobox.GetCount(), equal_to(len(note1_choices)))

        note2_combobox = self.get_child_by_name(undertest, AcceptSourceDialog.NOTE2)
        assert_that(note2_combobox.GetValue(), equal_to(""))
        assert_that(not note2_combobox.IsEditable())
        assert_that(note2_combobox.GetCount(), equal_to(len(note2_choices)))


if __name__ == '__main__':
    unittest.main()

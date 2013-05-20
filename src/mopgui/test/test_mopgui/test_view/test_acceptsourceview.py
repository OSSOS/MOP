TEST_DEC = 31.2123
TEST_RA = 27.213
__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from mopgui.view.acceptsourceview import AcceptSourceDialog

TEST_PROVISIONAL_NAME = "provisional-name-1"
TEST_DATE = "2012 01 01"


class AcceptSourceDialogTest(WxWidgetTestCase):
    def setUp(self):
        self.app = wx.App()
        self.rootframe = wx.Frame(None)

    def tearDown(self):
        self.rootframe.Destroy()

    def test_create_components(self):
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME,
                                       TEST_DATE, TEST_RA, TEST_DEC)

        component_labels = [AcceptSourceDialog.MINOR_PLANET_NUMBER, AcceptSourceDialog.PROVISIONAL_NAME,
                            AcceptSourceDialog.DISCOVERY_ASTERISK, AcceptSourceDialog.NOTE1,
                            AcceptSourceDialog.NOTE2, AcceptSourceDialog.DATE_OF_OBS,
                            AcceptSourceDialog.RA, AcceptSourceDialog.DEC, AcceptSourceDialog.OBS_MAG,
                            AcceptSourceDialog.BAND, AcceptSourceDialog.OBSERVATORY_CODE]

        for label in component_labels:
            self.assert_has_child_with_label(undertest, label)

    def test_required_preset_values(self):
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME,
                                       TEST_DATE, TEST_RA, TEST_DEC)

        assert_that(undertest.GetTitle(), equal_to(AcceptSourceDialog.TITLE))
        self.assert_has_child_with_label(undertest, TEST_PROVISIONAL_NAME)

        self.assert_has_child_with_label(undertest, str(TEST_RA))
        self.assert_has_child_with_label(undertest, str(TEST_DEC))
        self.assert_has_child_with_label(undertest, TEST_DATE)

    def test_note_comboboxes_populated(self):
        note1_choices = ["n1a", "n1b"]
        note2_choices = ["n2a", "n2b", "n2c"]
        undertest = AcceptSourceDialog(self.rootframe, TEST_PROVISIONAL_NAME,
                                       TEST_DATE, TEST_RA, TEST_DEC,
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

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, call
from hamcrest import assert_that, equal_to, has_length, contains

from tests.base_tests import WxWidgetTestCase
from ossos.gui.controllers import ProcessRealsController
from ossos.gui.views.validation import SourceValidationDialog, AcceptSourceDialog, RejectSourceDialog

# Constants used for test data
TEST_MINOR_PLANET_NUMBER = "mpn01"
TEST_PROVISIONAL_NAME = "provisional-name-1"
TEST_DISCOVERY_AST = "*"
TEST_NOTE1 = "A"
TEST_NOTE2 = "B"
TEST_DATE = "2012 01 01"
TEST_DEC = 31.2123
TEST_RA = 27.213
TEST_MAG = "123.5"
TEST_MAG_ERR = "5"
TEST_BAND = "A"
TEST_OBS_CODE = "523"
TEST_COMMENT = "Test comment"


class AcceptSourceDialogTest(WxWidgetTestCase):
    def setUp(self):
        super(AcceptSourceDialogTest, self).setUp()

        self.controller = Mock(spec=ProcessRealsController)

    def create_undertest(self, note1_choices=None, note2_choices=None):
        return AcceptSourceDialog(self.rootframe, self.controller,
                                  TEST_PROVISIONAL_NAME,
                                  TEST_DATE, TEST_RA, TEST_DEC, TEST_MAG,
                                  TEST_MAG_ERR, TEST_BAND,
                                  note1_choices=note1_choices,
                                  note2_choices=note2_choices)

    def test_create_components(self):
        undertest = self.create_undertest()

        component_labels = [AcceptSourceDialog.MINOR_PLANET_NUMBER, AcceptSourceDialog.PROVISIONAL_NAME,
                            AcceptSourceDialog.NOTE2, AcceptSourceDialog.DATE_OF_OBS,
                            AcceptSourceDialog.RA, AcceptSourceDialog.DEC, AcceptSourceDialog.OBS_MAG,
                            AcceptSourceDialog.BAND, AcceptSourceDialog.OBSERVATORY_CODE,
                            AcceptSourceDialog.SUBMIT_BTN, AcceptSourceDialog.CANCEL_BTN]

        for label in component_labels:
            self.assert_has_child_with_label(undertest, label)

    def test_required_preset_values(self):
        undertest = self.create_undertest()

        def assert_has_value(name, value):
            assert_that(self.get_child_by_name(undertest, name).GetValue(),
                        equal_to(str(value)))

        assert_that(undertest.GetTitle(), equal_to(AcceptSourceDialog.TITLE))
        assert_has_value(AcceptSourceDialog.PROVISIONAL_NAME, TEST_PROVISIONAL_NAME)

        assert_has_value(AcceptSourceDialog.RA, TEST_RA)
        assert_has_value(AcceptSourceDialog.DEC, TEST_DEC)
        assert_has_value(AcceptSourceDialog.DATE_OF_OBS, TEST_DATE)

    def test_note_comboboxes_populated(self):
        note1_choices = ["", "n1a", "n1b"]
        note2_choices = ["", "n2a", "n2b", "n2c"]
        undertest = self.create_undertest(note1_choices, note2_choices)

        note1_combobox = self.get_child_by_name(undertest, AcceptSourceDialog.NOTE1)
        assert_that(note1_combobox.GetValue(), equal_to(""))
        assert_that(note1_combobox.GetCount(), equal_to(len(note1_choices)))

        note2_combobox = self.get_child_by_name(undertest, AcceptSourceDialog.NOTE2)
        assert_that(note2_combobox.GetValue(), equal_to(""))
        assert_that(note2_combobox.GetCount(), equal_to(len(note2_choices)))

    def test_cancel_event(self):
        undertest = self.create_undertest()

        cancel_button = self.get_child_by_name(undertest, AcceptSourceDialog.CANCEL_BTN)
        self.fire_button_click_event(cancel_button)

        assert_that(self.controller.on_do_accept.call_args_list, has_length(0))
        assert_that(self.controller.on_cancel_accept.call_args_list, contains(call()))

    def test_submit_data(self):
        note1_choices = ["C", TEST_NOTE1]
        note2_choices = [TEST_NOTE2, "D", "E"]
        undertest = self.create_undertest(note1_choices, note2_choices)

        def get(name):
            """Convenience method"""
            return self.get_child_by_name(undertest, name)

        # Enter data
        get(AcceptSourceDialog.MINOR_PLANET_NUMBER).SetValue(TEST_MINOR_PLANET_NUMBER)
        get(AcceptSourceDialog.NOTE1).SetStringSelection(TEST_NOTE1)
        get(AcceptSourceDialog.NOTE2).SetStringSelection(TEST_NOTE2)
        get(AcceptSourceDialog.OBSERVATORY_CODE).SetValue(TEST_OBS_CODE)
        get(AcceptSourceDialog.COMMENT).SetValue(TEST_COMMENT)

        # Submit data
        ok_button = self.get_child_by_name(
            undertest, SourceValidationDialog.SUBMIT_BTN)
        self.fire_button_click_event(ok_button)

        # Check data
        assert_that(self.controller.on_cancel_accept.called, equal_to(False))
        self.controller.on_do_accept.assert_called_once_with(
            TEST_MINOR_PLANET_NUMBER, TEST_PROVISIONAL_NAME,
            TEST_NOTE1, TEST_NOTE2, TEST_DATE, str(TEST_RA), str(TEST_DEC), TEST_MAG,
            TEST_MAG_ERR, TEST_BAND, TEST_OBS_CODE, TEST_COMMENT)

    def test_submit_data_phot_failure(self):
        undertest = AcceptSourceDialog(self.rootframe, self.controller,
                                       TEST_PROVISIONAL_NAME,
                                       TEST_DATE, TEST_RA, TEST_DEC, TEST_MAG,
                                       TEST_MAG_ERR, TEST_BAND,
                                       note1_choices=None,
                                       note2_choices=None,
                                       default_observatory_code=TEST_OBS_CODE,
                                       default_comment=TEST_COMMENT,
                                       phot_failure=True
        )

        # Submit data
        ok_button = self.get_child_by_name(
            undertest, SourceValidationDialog.SUBMIT_BTN)
        self.fire_button_click_event(ok_button)

        # Check data
        obs_mag = ""
        obs_mag_err = -1
        band = ""
        assert_that(self.controller.on_cancel_accept.called, equal_to(False))
        self.controller.on_do_accept.assert_called_once_with(
            "", TEST_PROVISIONAL_NAME,
            "", "", TEST_DATE, str(TEST_RA), str(TEST_DEC), obs_mag,
            obs_mag_err, band, TEST_OBS_CODE, TEST_COMMENT)


class RejectSourceDialogTest(WxWidgetTestCase):
    def setUp(self):
        super(RejectSourceDialogTest, self).setUp()

        self.controller = Mock(spec=ProcessRealsController)
        self.undertest = RejectSourceDialog(self.rootframe, self.controller)

    def test_submit_comment(self):
        # Enter data
        self.get_child_by_name(
            self.undertest, RejectSourceDialog.COMMENT).SetValue(TEST_COMMENT)

        # Submit data
        ok_button = self.get_child_by_name(
            self.undertest, SourceValidationDialog.SUBMIT_BTN)
        self.fire_button_click_event(ok_button)

        self.controller.on_do_reject.assert_called_once_with(TEST_COMMENT)


if __name__ == '__main__':
    unittest.main()

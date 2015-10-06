"""
Base classes for test cases.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

import wx
from mock import Mock

from ossos.gui import events


class FileReadingTestCase(unittest.TestCase):
    def get_abs_path(self, relative_path):
        """
        Get the absolute path of a file in the test directory.

        Args:
          relative_path: str
            path of the file with respect to the base of the test directory
            ex: data/datafile.csv

        Returns:
          absolute_path: str
            The absolute path to the file.
        """
        if relative_path.startswith("/"):
            relative_path = relative_path[1:]
        relative_path.replace("/", os.sep)
        return os.path.join(os.path.dirname(__file__), relative_path)


class WxWidgetTestCase(unittest.TestCase):
    def setUp(self):
        events.unsub_all()
        self.app = wx.App(False)
        self.rootframe = wx.Frame(None)

    def tearDown(self):
        self.rootframe.Destroy()
        self.app.Exit()

    def _get_by_equality(self, iterable, is_equal):
        for item in iterable:
            if is_equal(item):
                return item
        return None

    def _get_by_label(self, label, iterable):
        def has_same_label(item):
            return item.GetLabel().lower() == label.lower()

        return self._get_by_equality(iterable, has_same_label)

    def assert_has_child_with_label(self, parent, label):
        self.assertIsNotNone(self.get_child_by_label(parent, label))

    def get_child_by_label(self, parent, label):
        return self._get_by_label(label, parent.GetChildren())

    def get_child_by_name(self, parent, name):
        def has_same_name(item):
            return item.GetName() == name

        return self._get_by_equality(parent.GetChildren(), has_same_name)

    def get_menuitem_by_label(self, menu, label):
        return self._get_by_label(label, menu.GetMenuItems())

    def fire_button_click_event(self, widget):
        event = wx.CommandEvent(wx.wxEVT_COMMAND_BUTTON_CLICKED,
                                widget.GetId())
        widget.GetEventHandler().ProcessEvent(event)

    def mock_model(self):
        self.model = Mock()
        self.model.get_reading_data.return_value = (("TEST1", 1), ("TEST2", 2))
        self.model.get_header_data_list.return_value = [("Key1", "Val1"), ("Key2", "Val2")]
        self.model.get_current_image_source_point.return_value = (0, 0)
        self.model.get_current_image_FWHM.return_value = 3
        self.model.get_current_source_number.return_value = 0
        self.model.get_source_count.return_value = 2
        self.model.get_current_obs_number.return_value = 0
        self.model.get_obs_count.return_value = 3


class DirectoryCleaningTestCase(unittest.TestCase):
    def tearDown(self):
        directory = self.get_directory_to_clean()
        for filename in os.listdir(directory):
            if filename not in self.get_files_to_keep():
                os.remove(os.path.join(directory, filename))

    def get_directory_to_clean(self):
        raise NotImplementedError()

    def get_files_to_keep(self):
        raise NotImplementedError()

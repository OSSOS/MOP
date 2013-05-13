"""
Base classes for test cases.
"""

import os
import unittest

import wx


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
    def _get_by_label(self, label, iterable):
        for child in iterable:
            if child.GetLabel().lower() == label.lower():
                return child
        return None

    def get_child_by_label(self, parent, label):
        return self._get_by_label(label, parent.GetChildren())

    def get_menuitem_by_label(self, menu, label):
        return self._get_by_label(label, menu.GetMenuItems())

    def fire_button_click_event(self, widget):
        event = wx.CommandEvent(wx.wxEVT_COMMAND_BUTTON_CLICKED,
                                widget.GetId())
        widget.GetEventHandler().ProcessEvent(event)

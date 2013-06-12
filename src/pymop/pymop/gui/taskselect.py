"""
Self-contained graphical utility for gathering task setup information from
the user.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.filebrowsebutton as filebrowse

from pymop import config


class WorkingDirectorySelectionFrame(wx.Frame):
    def __init__(self, selector):
        super(WorkingDirectorySelectionFrame, self).__init__(
            None, title=config.read("UI.APPTITLE"))

        self.selector = selector

        self._init_ui()
        self._bind_events()

    def _init_ui(self):
        # self.panel = wx.Panel(self)

        self.header = wx.StaticText(self, -1, "Select working directory:")
        self.header.SetFont(wx.Font(18, wx.FONTFAMILY_SWISS, wx.FONTSTYLE_NORMAL,
                                    wx.FONTWEIGHT_BOLD))

        self.browser = filebrowse.DirBrowseButton(
            self, -1, labelText="", dialogTitle="Select working directory")

        self.ok_button = wx.Button(self, -1, label="Ok")

        self._do_layout()

    def _do_layout(self):
        border = 5

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.header, 0, wx.ALIGN_CENTRE | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)
        vsizer.AddSpacer(20)
        vsizer.Add(self.browser, 0, wx.EXPAND | wx.ALL, border)
        vsizer.Add(self.ok_button, 0, wx.ALIGN_CENTER | wx.ALL, border)

        self.SetSizer(vsizer)

    def _bind_events(self):
        self.ok_button.Bind(wx.EVT_BUTTON, self._on_ok)

    def _on_ok(self, event):
        self.selector.set_working_directory(self.browser.GetValue())


class WorkingDirectorySelector(object):
    def __init__(self, app):
        """
        Prompts the user to enter the desired working directory.

        Args:
          app: PymopApplication
            The application this selection is being made for.

        Returns:
          working_directory: str
            The user selected working directory.
        """
        self.app = app

    def run(self):
        self.selection_frame = WorkingDirectorySelectionFrame(self)
        self.selection_frame.Show()

    def set_working_directory(self, working_directory):
        """
        Prompts the user to enter the desired working directory.

        Args:
          wx_app: wx.App
            The wx application which the GUI components should be a part of.

        Returns:
          working_directory: str
            The user selected working directory.
        """
        self.selection_frame.Close()
        self.app.set_working_directory(working_directory)

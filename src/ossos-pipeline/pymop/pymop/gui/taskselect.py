"""
Self-contained graphical utility for gathering task setup information from
the user.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.filebrowsebutton as filebrowse

from pymop import config
from pymop import tasks


class TaskSetupFrame(wx.Frame):
    # CANDS_LABEL = "Process candidate objects"
    # REALS_LABEL = "Process real objects"
    OK_LABEL = "Ok"

    def __init__(self, setup_manager):
        super(TaskSetupFrame, self).__init__(
            None, title=config.read("UI.APPTITLE"))

        self.setup_manager = setup_manager

        self._init_ui()
        self._bind_events()

    def _init_ui(self):
        self.task_header = wx.StaticText(self, -1, "Select a task:")

        self.radiobox = wx.RadioBox(self, -1, "", choices=tasks.task_list,
                                    style=wx.RA_VERTICAL | wx.NO_BORDER)

        self.working_dir_header = wx.StaticText(self, -1, "Select working directory:")

        self.browser = filebrowse.DirBrowseButton(
            self, -1, labelText="", dialogTitle="Select working directory")

        self.ok_button = wx.Button(self, -1, label=TaskSetupFrame.OK_LABEL)

        self._do_layout()

    def _do_layout(self):
        border = 5

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.task_header, 0, wx.ALIGN_CENTRE | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)
        vsizer.Add(self.radiobox, 0, wx.ALL, border)
        # vsizer.Add(self.process_reals_option, 0, wx.ALL, border)
        vsizer.AddSpacer(20)
        vsizer.Add(self.working_dir_header, 0, wx.ALIGN_CENTER | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)
        vsizer.Add(self.browser, 0, wx.EXPAND | wx.ALL, border)
        vsizer.Add(self.ok_button, 0, wx.ALIGN_CENTER | wx.ALL, border)

        self.SetSizer(vsizer)

    def _bind_events(self):
        self.ok_button.Bind(wx.EVT_BUTTON, self._on_ok)

    def _on_ok(self, event):
        task = self.radiobox.GetStringSelection()
        self.setup_manager.set_task_info(self.browser.GetValue(), task)


class TaskSetupManager(object):
    def __init__(self, app):
        """
        Prompts the user to enter the desired working directory and select
        a task.

        Args:
          pymopapp: PymopApplication
            The application this selection is being made for.
        """
        self.app = app

    def run(self):
        self.selection_frame = TaskSetupFrame(self)
        self.selection_frame.Show()

    def set_task_info(self, working_directory, task):
        self.selection_frame.Close()
        self.app.set_task_info(working_directory, task)

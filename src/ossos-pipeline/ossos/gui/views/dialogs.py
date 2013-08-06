"""
This module contains various dialogs used in the application.
The accept/reject source dialogs are in validation.py.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import config


class WaitingGaugeDialog(wx.Dialog):
    def __init__(self, parent, wait_message, pulse_period_ms=100):
        super(WaitingGaugeDialog, self).__init__(parent)

        self.wait_message = wait_message
        self.pulse_period_ms = pulse_period_ms

        self._init_ui()

    def _init_ui(self):
        # Non-visible component used to periodically update gauge
        self.timer = wx.Timer(self)

        # Create visible components
        self.msg = wx.StaticText(self, label=self.wait_message)
        self.gauge = wx.Gauge(self)
        self.hidebutton = wx.Button(self, label="Hide")

        self._do_layout()

        self.hidebutton.Bind(wx.EVT_BUTTON, self._on_hide)
        self.Bind(wx.EVT_TIMER, self._on_tick, self.timer)

        self.timer.Start(self.pulse_period_ms)

    def _do_layout(self):
        vborder = 10
        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.msg, flag=wx.CENTER | wx.TOP | wx.BOTTOM, border=vborder)
        vsizer.Add(self.gauge, flag=wx.EXPAND | wx.TOP | wx.BOTTOM, border=vborder)
        vsizer.Add(self.hidebutton, flag=wx.CENTER | wx.TOP | wx.BOTTOM, border=vborder)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(vsizer, flag=wx.LEFT | wx.RIGHT, border=20)

        self.SetSizer(hsizer)
        hsizer.Fit(self)

    def _on_tick(self, event):
        self.gauge.Pulse()

    def _on_hide(self, event):
        self.Hide()


def should_exit_prompt(parent):
    dialog = wx.MessageDialog(parent,
                              config.read("UI.ALLPROC.MSG"),
                              caption=config.read("UI.ALLPROC.CAPTION"),
                              style=wx.YES_NO | wx.ICON_INFORMATION)

    user_choice = dialog.ShowModal()
    dialog.Destroy()

    return True if user_choice == wx.ID_YES else False


def show_empty_workload_dialog(parent, model):
    message = ("No work to be done in %s\n"
               "It was either already processed or has no input files "
               "for the selected task." % model.get_working_directory())
    dialog = wx.MessageDialog(parent,
                              message,
                              caption="Empty Workload",
                              style=wx.OK | wx.ICON_INFORMATION)

    dialog.ShowModal()
    dialog.Destroy()
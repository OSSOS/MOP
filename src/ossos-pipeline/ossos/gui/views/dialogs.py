"""
This module contains various dialogs used in the application.
The accept/reject source dialogs are in validation.py.
"""
__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import config
from ossos.gui.views.listctrls import ListCtrlPanel


def should_exit_prompt(parent):
    dialog = wx.MessageDialog(parent,
                              config.read("UI.ALLPROC.MSG"),
                              caption=config.read("UI.ALLPROC.CAPTION"),
                              style=wx.YES_NO | wx.ICON_INFORMATION)

    user_choice = dialog.ShowModal()
    dialog.Destroy()

    return True if user_choice == wx.ID_YES else False


def show_empty_workload_dialog(parent, directory):
    message = ("No work to be done in %s\n"
               "It was either already processed or has no input files "
               "for the selected task." % directory)
    dialog = wx.MessageDialog(parent,
                              message,
                              caption="Empty Workload",
                              style=wx.OK | wx.ICON_INFORMATION)

    dialog.ShowModal()
    dialog.Destroy()


def show_keymappings_dialog(parent, keybind_manager):
    dialog = wx.Dialog(parent, title="Key Mappings")
    panel = ListCtrlPanel(dialog, ("Action", "Shortcut"))
    panel.populate_list(keybind_manager.get_keymappings())

    dialog.Show()
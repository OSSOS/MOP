__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from pymop import config


def should_exit_prompt(parent):
    dialog = wx.MessageDialog(parent,
                              config.read("UI.ALLPROC.MSG"),
                              caption=config.read("UI.ALLPROC.CAPTION"),
                              style=wx.YES_NO | wx.ICON_INFORMATION)

    user_choice = dialog.ShowModal()
    dialog.Destroy()

    return True if user_choice == wx.ID_YES else False

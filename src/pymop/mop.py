#!/usr/bin/env python

__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from pymop.applauncher import AstromFileApplicationLauncher
from pymop.gui.launcher import LaunchWizardManager


def main():
    app = wx.App()
    LaunchWizardManager(AstromFileApplicationLauncher())
    app.MainLoop()


if __name__ == "__main__":
    main()

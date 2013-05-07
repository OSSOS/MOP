"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx
import wx.lib.inspection

from mopgui.view import util


class ApplicationView(object):
    def __init__(self):
        self.wx_app = wx.App(False)
        self.mainframe = MainFrame()

    def launch(self, debug_mode=False):
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        self.mainframe.Show()
        self.wx_app.MainLoop()


class MainFrame(wx.Frame):
    def __init__(self):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

        self.SetIcon(wx.Icon(util.get_asset_full_path("cadc_icon.jpg"),
                             wx.BITMAP_TYPE_JPEG))

        self._create_menus()

    def _create_menus(self):

        def do_bind(handler, item):
            self.Bind(wx.EVT_MENU, handler, item)

        # Create menus and their contents
        file_menu = wx.Menu()
        exit_item = file_menu.Append(wx.ID_EXIT, "Exit", "Exit the program")
        do_bind(self.on_exit, exit_item)

        # Create menu bar
        menubar = wx.MenuBar()
        menubar.Append(file_menu, "File")
        self.SetMenuBar(menubar)

    def on_exit(self, event):
        self.Close()

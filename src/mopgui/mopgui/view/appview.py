"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx
import wx.lib.inspection

from mopgui.view import util


class ApplicationView(object):
    def __init__(self, astrom_data, image_viewer):
        self.astrom_data = astrom_data
        self.image_viewer = image_viewer

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame()
        self.mainframe.subscribe(self)

        self.current_source = 0
        self.current_observation = 0

    def _view_current_image(self):
        current_image = self.astrom_data.sources[self.current_source][self.current_observation].image
        self.image_viewer.view_image(current_image)
        # Add 1 so displayed source numbers don't start at 0
        self.mainframe.set_source_status(
            self.current_source + 1, len(self.astrom_data.sources))

    def launch(self, debug_mode=False):
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        self._view_current_image()

        self.mainframe.Show()
        self.wx_app.MainLoop()

    def on_next_source(self):
        if self.current_source + 1 < len(self.astrom_data.sources):
            self.current_source += 1
        self._view_current_image()

    def on_previous_source(self):
        if self.current_source > 0:
            self.current_source -= 1
        self._view_current_image()

    def on_exit(self):
        self.image_viewer.close()


class MainFrame(wx.Frame):
    def __init__(self):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

        self.SetIcon(wx.Icon(util.get_asset_full_path("cadc_icon.jpg"),
                             wx.BITMAP_TYPE_JPEG))

        self._create_menus()
        self.CreateStatusBar()

        self._create_navigation_controls()

        self.event_subscribers = []

    def subscribe(self, subscriber):
        self.event_subscribers.append(subscriber)

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

    def _create_navigation_controls(self):
        next_source_button = wx.Button(self, wx.ID_FORWARD,
                                       label="Next Source")
        next_source_button.Bind(wx.EVT_BUTTON, self.on_next_source)

        previous_source_button = wx.Button(self, wx.ID_BACKWARD,
                                           label="Previous Source")
        previous_source_button.Bind(wx.EVT_BUTTON, self.on_previous_source)

        # Layout
        source_button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        source_button_sizer.Add(previous_source_button)
        source_button_sizer.Add(next_source_button)

        self.SetSizer(source_button_sizer)

    def set_source_status(self, current_source, total_sources):
        self.GetStatusBar().SetStatusText(
            "Source %d of %d" % (current_source, total_sources))

    def on_exit(self, event):
        self.Close()
        # TODO refactor
        for subscriber in self.event_subscribers:
            subscriber.on_exit()

    def on_next_source(self, event):
        # TODO refactor
        for subscriber in self.event_subscribers:
            subscriber.on_next_source()

    def on_previous_source(self, event):
        # TODO refactor
        for subscriber in self.event_subscribers:
            subscriber.on_previous_source()

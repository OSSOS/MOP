"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx
import wx.lib.inspection

from mopgui.view import util
from mopgui.view.list_ctrl import ListCtrlPanel


class ApplicationView(object):
    def __init__(self, astrom_data, image_viewer):
        self.astrom_data = astrom_data
        self.image_viewer = image_viewer

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame()
        self.mainframe.subscribe(self)

        self.current_source = 0
        self.current_observation = 0

        self._init_ui()

    def _init_ui(self):
        # TODO refactor
        reading = self._get_current_reading()
        reading_data_list = [("X", str(reading.x)), ("Y", str(reading.y)),
                             ("X_0", str(reading.x0)), ("Y_0", str(reading.y0)),
                             ("RA", str(reading.ra)), ("DEC", str(reading.dec))]
        self.mainframe.notebook.reading_data_panel.populate_list(reading_data_list)

        header_data_list = [(key, value) for key, value in reading.obs.header.iteritems()]
        self.mainframe.notebook.obs_header_panel.populate_list(header_data_list)

    def _get_current_reading(self):
        return self.astrom_data.sources[self.current_source][self.current_observation]

    def _view_current_image(self):
        current_reading = self._get_current_reading()
        self.image_viewer.view_image(current_reading.image)

        image_x, image_y = current_reading.image_source_point
        radius = 2 * round(float(current_reading.obs.header["FWHM"]))
        self.image_viewer.draw_circle(image_x, image_y, radius)

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

        self._init_ui_components()

        self.event_subscribers = []

    def _init_ui_components(self):
        self.menubar = self._create_menus()
        self.CreateStatusBar()
        self.nav_ctrls = self._create_navigation_controls()

        self.notebook = MainNotebook(self)

        # Layout
        main_component_sizer = wx.BoxSizer(wx.VERTICAL)
        main_component_sizer.Add(self.nav_ctrls, 0, flag=wx.ALIGN_TOP |
                                                         wx.ALIGN_CENTER)
        main_component_sizer.Add(self.notebook, 1, flag=wx.EXPAND)

        self.SetSizer(main_component_sizer)

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

        return menubar

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

        return source_button_sizer

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


class MainNotebook(wx.Notebook):
    def __init__(self, parent):
        super(MainNotebook, self).__init__(parent)

        self._init_ui_components()

    def _init_ui_components(self):
        self.reading_data_panel = ListCtrlPanel(self, ("Key", "Value"))
        self.obs_header_panel = ListCtrlPanel(self, ("Key", "Value"))

        self.AddPage(self.reading_data_panel, "Readings")
        self.AddPage(self.obs_header_panel, "Observation Header")


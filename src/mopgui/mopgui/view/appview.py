"""
The entry-point for the "view" of the Model-View-Controller.
"""

import wx
from wx.lib.pubsub import Publisher as pub
import wx.lib.inspection

from mopgui.view import util, navview
from mopgui.view.list_ctrl import ListCtrlPanel
from mopgui.model import astrodata


class ApplicationView(object):
    def __init__(self, model, image_viewer):
        self.model = model

        self.image_viewer = image_viewer

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame(model)
        # TODO refactor
        self.mainframe.subscribe(self)

        self._init_ui()

        pub.subscribe(self._view_current_image, astrodata.MSG_NAV)

    def _init_ui(self):
        # TODO refactor
        reading_data_list = self.model.get_reading_data()
        self.mainframe.notebook.reading_data_panel.populate_list(reading_data_list)

        reading = self.model._get_current_reading()
        header_data_list = [(key, value) for key, value in reading.obs.header.iteritems()]
        self.mainframe.notebook.obs_header_panel.populate_list(header_data_list)

    def _view_current_image(self, event):
        self.image_viewer.view_image(self.model.get_current_image())

        # TODO refactor
        current_reading = self.model._get_current_reading()
        image_x, image_y = current_reading.image_source_point
        radius = 2 * round(float(current_reading.obs.header["FWHM"]))
        self.image_viewer.draw_circle(image_x, image_y, radius)

        # Add 1 so displayed source numbers don't start at 0
        self.mainframe.set_source_status(
            self.model.get_current_source_number() + 1,
            self.model.get_source_count())

    def launch(self, debug_mode=False):
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        self._view_current_image(None)

        self.mainframe.Show()
        self.wx_app.MainLoop()

    def on_exit(self):
        self.image_viewer.close()


class MainFrame(wx.Frame):
    def __init__(self, model):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

        self.SetIcon(wx.Icon(util.get_asset_full_path("cadc_icon.jpg"),
                             wx.BITMAP_TYPE_JPEG))

        self.model = model

        self.event_subscribers = []

        self._init_ui_components()

    def subscribe(self, subscriber):
        self.event_subscribers.append(subscriber)

    def _init_ui_components(self):
        self.menubar = self._create_menus()
        self.CreateStatusBar()
        self.nav_ctrls = self._create_navigation_controls()

        self.notebook = MainNotebook(self)

        # Layout
        main_component_sizer = wx.BoxSizer(wx.VERTICAL)
        main_component_sizer.Add(self.nav_ctrls, 0, flag=wx.ALIGN_TOP |
                                                         wx.ALIGN_CENTER)
        main_component_sizer.Add(self.notebook, 2, flag=wx.EXPAND)

        self.SetSizer(main_component_sizer)

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
        # return NavPanel(self)
        return navview.NavPanel(self, self.model)

    def set_source_status(self, current_source, total_sources):
        self.GetStatusBar().SetStatusText(
            "Source %d of %d" % (current_source, total_sources))

    def on_exit(self, event):
        self.Close()
        # TODO refactor
        for subscriber in self.event_subscribers:
            subscriber.on_exit()


class MainNotebook(wx.Notebook):
    def __init__(self, parent):
        super(MainNotebook, self).__init__(parent)

        self._init_ui_components()

    def _init_ui_components(self):
        self.reading_data_panel = ListCtrlPanel(self, ("Key", "Value"))
        self.obs_header_panel = ListCtrlPanel(self, ("Key", "Value"))

        self.AddPage(self.reading_data_panel, "Readings")
        self.AddPage(self.obs_header_panel, "Observation Header")


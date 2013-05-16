"""
The entry-point for the "view" of the Model-View-Controller.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
from wx.lib.pubsub import Publisher as pub
import wx.lib.inspection

from mopgui.view import util, wxutil, navview, dialogs
from mopgui.view.dataview import ReadingDataView, ObservationHeaderView
from mopgui.model import astrodata


class ApplicationView(object):
    def __init__(self, model, appcontroller, navcontroller):
        self.model = model

        self.appcontroller = appcontroller
        self.navcontroller = navcontroller

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame(model, appcontroller, navcontroller)

        # TODO: move to controller Set up event subscriptions
        pub.subscribe(self.appcontroller.on_change_image, astrodata.MSG_NAV)
        pub.subscribe(self.appcontroller.on_image_loaded, astrodata.MSG_IMG_LOADED)

    def launch(self, debug_mode=False, unittest=False):
        wx.CallAfter(self.mainframe.show_image_loading_dialog)
        wx.CallAfter(self.model.start_loading_images)

        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        if not unittest:
            self.mainframe.Show()
            self.wx_app.MainLoop()

    def close(self):
        self.mainframe.Close()

    @wxutil.threadsafe
    def show_image_loading_dialog(self):
        self.mainframe.show_image_loading_dialog()

    @wxutil.threadsafe
    def hide_image_loading_dialog(self):
        self.mainframe.hide_image_loading_dialog()

    @wxutil.threadsafe
    def set_source_status(self, current_source, total_sources):
        self.mainframe.set_source_status(current_source, total_sources)


class MainFrame(wx.Frame):
    def __init__(self, model, appcontroller, navcontroller, size=(400, 500)):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline",
                                        size=size)

        self.SetIcon(wx.Icon(util.get_asset_full_path("cadc_icon.jpg"),
                             wx.BITMAP_TYPE_JPEG))

        self.model = model

        self.appcontroller = appcontroller
        self.navcontroller = navcontroller

        self._init_ui_components()

    def _init_ui_components(self):
        self.menubar = self._create_menus()

        self.CreateStatusBar()
        self.nav_view = navview.NavPanel(self, self.navcontroller)

        self.notebook = MainNotebook(self, self.model)

        # Layout
        main_component_sizer = wx.BoxSizer(wx.VERTICAL)
        main_component_sizer.Add(self.nav_view, 0, flag=wx.ALIGN_TOP |
                                                        wx.ALIGN_CENTER)
        main_component_sizer.Add(self.notebook, 2, flag=wx.EXPAND)

        self.SetSizer(main_component_sizer)

        self.img_loading_dialog = dialogs.WaitingGaugeDialog(self, "Image loading...")

    def _create_menus(self):
        def do_bind(handler, item):
            self.Bind(wx.EVT_MENU, handler, item)

        # Create menus and their contents
        file_menu = wx.Menu()
        exit_item = file_menu.Append(wx.ID_EXIT, "Exit", "Exit the program")
        do_bind(self.appcontroller.on_exit, exit_item)

        # Create menu bar
        menubar = wx.MenuBar()
        menubar.Append(file_menu, "File")
        self.SetMenuBar(menubar)

        return menubar

    def show_image_loading_dialog(self):
        if not self.img_loading_dialog.IsShown():
            self.img_loading_dialog.CenterOnParent()
            self.img_loading_dialog.Show()

    def hide_image_loading_dialog(self):
        if self.img_loading_dialog.IsShown():
            self.img_loading_dialog.Hide()

    def set_source_status(self, current_source, total_sources):
        self.GetStatusBar().SetStatusText(
            "Source %d of %d" % (current_source, total_sources))


class MainNotebook(wx.Notebook):
    def __init__(self, parent, model):
        super(MainNotebook, self).__init__(parent)

        self.model = model

        self._init_ui_components()

    def _init_ui_components(self):
        self.reading_data_panel = ReadingDataView(self, self.model)
        self.obs_header_panel = ObservationHeaderView(self, self.model)

        self.AddPage(self.reading_data_panel, "Readings")
        self.AddPage(self.obs_header_panel, "Observation Header")


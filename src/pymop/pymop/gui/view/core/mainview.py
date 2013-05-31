__author__ = "David Rusk <drusk@uvic.ca>"

import wx
from wx.lib.pubsub import Publisher as pub

from pymop.gui import models
from pymop.gui.view.core.validationview import SourceValidationPanel
from pymop.gui.view.core.listview import KeyValueListPanel
from pymop.gui.view.core.statusview import AppStatusBar
from pymop.gui.view.core import util, navview, dialogs
from pymop.gui.view.image.mplview import MPLImageViewer


class MainFrame(wx.Frame):
    """
    This is the main window of the application.  It should not be
    manipulated externally (it should be considered an implementation detail
    of the ApplicationView).  Therefore, updates should come from the
    ApplicationView.
    """

    def __init__(self, model, controller, size=(1000, 500)):
        # TODO put default size in config
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline",
                                        size=size)

        self.SetIcon(wx.Icon(util.get_asset_full_path("cadc_icon.jpg"),
                             wx.BITMAP_TYPE_JPEG))

        self.model = model

        self.controller = controller

        self._init_ui_components()

    def _init_ui_components(self):
        self.menubar = self._create_menus()

        self.main_panel = wx.Panel(self, style=wx.RAISED_BORDER)
        self.control_panel = wx.Panel(self.main_panel)

        self.nav_view = navview.NavPanel(self.control_panel, self.controller)

        self.data_view = self._create_data_notebook()

        self.validation_view = SourceValidationPanel(self.control_panel, self.controller)

        self.viewer_panel = wx.Panel(self.main_panel, style=wx.RAISED_BORDER)
        self.image_viewer = MPLImageViewer(self.viewer_panel)

        self.statusbar = AppStatusBar(self)
        self.SetStatusBar(self.statusbar)

        self.img_loading_dialog = dialogs.WaitingGaugeDialog(self, "Image loading...")

        self._do_layout()

    def _do_layout(self):
        control_sizer = wx.BoxSizer(wx.VERTICAL)
        control_sizer.Add(self.nav_view, 1, flag=wx.EXPAND)
        control_sizer.Add(self.data_view, 2, flag=wx.EXPAND)
        control_sizer.Add(self.validation_view, 1, flag=wx.EXPAND)
        self.control_panel.SetSizerAndFit(control_sizer)

        main_sizer = wx.BoxSizer(wx.HORIZONTAL)
        main_sizer.Add(self.control_panel, flag=wx.EXPAND)
        main_sizer.Add(self.viewer_panel, flag=wx.EXPAND)

        self.main_panel.SetSizerAndFit(main_sizer)

    def _create_menus(self):
        def do_bind(handler, item):
            self.Bind(wx.EVT_MENU, handler, item)

        # Create menus and their contents
        file_menu = wx.Menu()
        exit_item = file_menu.Append(wx.ID_EXIT, "Exit", "Exit the program")
        do_bind(self.controller.on_exit, exit_item)

        # Create menu bar
        menubar = wx.MenuBar()
        menubar.Append(file_menu, "File")
        self.SetMenuBar(menubar)

        return menubar

    def _create_data_notebook(self):
        notebook = wx.Notebook(self.control_panel)

        reading_data_panel = KeyValueListPanel(notebook, self.model.get_reading_data)
        pub.subscribe(reading_data_panel.on_change_data, models.MSG_NAV)

        obs_header_panel = KeyValueListPanel(notebook, self.model.get_header_data_list)
        pub.subscribe(obs_header_panel.on_change_data, models.MSG_NAV)

        notebook.AddPage(reading_data_panel, "Readings")
        notebook.AddPage(obs_header_panel, "Observation Header")

        return notebook

    def view_image(self, img):
        self.image_viewer.view_image(img)

    def draw_circle(self, x, y, radius):
        self.image_viewer.draw_circle(x, y, radius)

    def show_image_loading_dialog(self):
        if not self.img_loading_dialog.IsShown():
            self.img_loading_dialog.CenterOnParent()
            self.img_loading_dialog.Show()

    def hide_image_loading_dialog(self):
        if self.img_loading_dialog.IsShown():
            self.img_loading_dialog.Hide()

    def set_source_status(self, current_source, total_sources):
        self.statusbar.set_source_status(current_source, total_sources)

    def set_loading_status(self, loaded, total):
        self.statusbar.set_loading_status(loaded, total)

    def set_observation_status(self, current_obs, total_obs):
        self.nav_view.set_status(current_obs, total_obs)

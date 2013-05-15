"""
The entry-point for the "view" of the Model-View-Controller.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
from wx.lib.pubsub import Publisher as pub
import wx.lib.inspection

from mopgui.view import util, navview, dialogs
from mopgui.view.dataview import ReadingDataView, ObservationHeaderView
from mopgui.model import astrodata


class ApplicationView(object):
    def __init__(self, model, appcontroller, navcontroller, image_viewer):
        self.model = model

        self.appcontroller = appcontroller
        self.navcontroller = navcontroller

        self.image_viewer = image_viewer

        self.wx_app = wx.App(False)
        self.mainframe = MainFrame(model, appcontroller, navcontroller)

        # Set up event subscriptions
        pub.subscribe(self.appcontroller.on_change_image, astrodata.MSG_NAV)

    def display_current_image(self):
        current_image = self.model.get_current_image()

        if current_image is None:
            self.mainframe.show_image_loading_dialog()
        else:
            self.image_viewer.view_image(current_image)
            image_x, image_y = self.model.get_current_image_source_point()
            radius = 2 * round(self.model.get_current_image_FWHM())
            self.image_viewer.draw_circle(image_x, image_y, radius)

        # Add 1 so displayed source numbers don't start at 0
        self.mainframe.set_source_status(
            self.model.get_current_source_number() + 1,
            self.model.get_source_count())

    def launch(self, debug_mode=False, unittest=False):
        if debug_mode:
            wx.lib.inspection.InspectionTool().Show()

        if not unittest:
            self.mainframe.Show()

            # Do this after showing the rest of the UI, but before getting
            # blocked by the mainloop
            self.display_current_image()

            self.wx_app.MainLoop()

    def close(self):
        self.image_viewer.close()
        self.mainframe.Close()


class MainFrame(wx.Frame):
    def __init__(self, model, appcontroller, navcontroller):
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline")

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
        dialog = dialogs.WaitingGaugeDialog(self, "Image loading...")
        dialog.ShowModal()

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


__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import config
from ossos.fitsviewer.singletviewer import SingletViewer
from ossos.gui.views.dialogs import WaitingGaugeDialog
from ossos.gui.views.keybinds import KeybindManager
from ossos.gui.views.listctrls import ListCtrlPanel
from ossos.gui.views.menu import Menu
from ossos.gui.views.navigation import NavPanel
from ossos.gui.views.validation import SourceValidationPanel


class MainFrame(wx.Frame):
    """
    This is the main window of the application.  It should not be
    manipulated externally (it should be considered an implementation detail
    of the ApplicationView).  Therefore, updates should come from the
    ApplicationView.
    """

    def __init__(self, model, controller):
        size = (config.read("UI.DIMENSIONS.WIDTH"),
                config.read("UI.DIMENSIONS.HEIGHT"))
        super(MainFrame, self).__init__(None, title="Moving Object Pipeline",
                                        size=size)

        self.model = model

        self.controller = controller

        self._init_ui_components()

        self.keybind_manager = KeybindManager(self, self.controller)

        # needed for keybinds to work on startup
        self.main_panel.SetFocus()

    def _init_ui_components(self):
        self.menu = Menu(self, self.controller)

        self.main_panel = _FocusablePanel(self, style=wx.RAISED_BORDER)
        self.control_panel = wx.Panel(self.main_panel)
        self.main_panel.use_as_focus(self.control_panel)

        self.nav_view = NavPanel(self.control_panel, self.controller)

        self.data_view = self._create_data_notebook()

        self.validation_view = SourceValidationPanel(self.control_panel, self.controller)

        self.image_viewer = SingletViewer(self.main_panel)

        self.img_loading_dialog = WaitingGaugeDialog(self, "Image loading...")

        self._do_layout()

    def _do_layout(self):
        control_sizer = wx.BoxSizer(wx.VERTICAL)
        control_sizer.Add(self.nav_view, 1, flag=wx.EXPAND)
        control_sizer.Add(self.data_view, 2, flag=wx.EXPAND)
        control_sizer.Add(self.validation_view, 1, flag=wx.EXPAND)
        self.control_panel.SetSizerAndFit(control_sizer)

        main_sizer = wx.BoxSizer(wx.HORIZONTAL)
        main_sizer.Add(self.control_panel, flag=wx.EXPAND)
        main_sizer.Add(self.image_viewer.as_widget(), flag=wx.EXPAND)

        self.main_panel.SetSizerAndFit(main_sizer)

    def _create_data_notebook(self):
        notebook = wx.Notebook(self.control_panel)

        columns = ("Key", "Value")
        self.reading_data_panel = ListCtrlPanel(notebook, columns)
        self.obs_header_panel = ListCtrlPanel(notebook, columns)

        notebook.AddPage(self.reading_data_panel, "Readings")
        notebook.AddPage(self.obs_header_panel, "Observation Header")

        return notebook

    def display(self, fits_image, redraw=True):
        self.image_viewer.display(fits_image, redraw=redraw)

    def draw_marker(self, x, y, radius, redraw=True):
        self.image_viewer.draw_marker(x, y, radius, redraw=redraw)

    def update_displayed_data(self):
        self.reading_data_panel.populate_list(self.model.get_reading_data())
        self.obs_header_panel.populate_list(self.model.get_header_data_list())

    def reset_colormap(self):
        self.image_viewer.reset_colormap()

    def register_xy_changed_event_handler(self, handler):
        self.image_viewer.register_xy_changed_event_handler(handler)

    def show_image_loading_dialog(self):
        if not self.img_loading_dialog.IsShown():
            self.img_loading_dialog.CenterOnParent()
            self.img_loading_dialog.Show()

    def hide_image_loading_dialog(self):
        if self.img_loading_dialog.IsShown():
            self.img_loading_dialog.Hide()

    def set_observation_status(self, current_obs, total_obs):
        self.nav_view.set_status(current_obs, total_obs)

    def disable_validation(self):
        self.validation_view.disable()

    def enable_validation(self):
        self.validation_view.enable()

    def is_source_validation_enabled(self):
        return self.validation_view.is_validation_enabled()

    def disable_sync_menu(self):
        self.menu.disable_sync()

    def set_autoplay(self, autoplay_enabled):
        self.menu.set_autoplay(autoplay_enabled)


class _FocusablePanel(wx.Panel):
    """
    Work-around used to make sure the right windows have focus so the
    keybind accelerator table works.
    """

    def __init__(self, *args, **kwargs):
        super(_FocusablePanel, self).__init__(*args, **kwargs)

        self._focus = None

    def use_as_focus(self, widget):
        self._focus = widget

    def SetFocus(self):
        """
        Over-rides normal behaviour of shifting focus to any child.  Prefers
        the one set explicityly by use_as_focus.
        """
        if self._focus is not None:
            self._focus.SetFocus()
        else:
            # fall back on the default behaviour
            super(_FocusablePanel, self).SetFocus()

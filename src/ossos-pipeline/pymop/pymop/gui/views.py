__author__ = "David Rusk <drusk@uvic.ca>"

import os

import wx

# TODO: upgrade
from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

from wx.lib.mixins import listctrl as listmix

from pymop import config
from pymop.gui import models
from pymop.gui.imgviewer import MPLImageViewer


def guithread(function):
    """
    Decorator to make sure the function is called from wx's main GUI
    thread.  This is needed when trying to trigger UI updates from a thread
    other than the main GUI thread (such as some asynchronous data loading
    thread).

    I learned about doing this from:
    wxPython 2.8 Application Development Cookbook, Chapter 11
    """

    def new_guithread_function(*args, **kwargs):
        if wx.Thread_IsMain():
            return function(*args, **kwargs)
        else:
            wx.CallAfter(function, *args, **kwargs)

    return new_guithread_function


class ApplicationView(object):
    """
    Provides the view's external interface.
    """

    def __init__(self, model, controller):
        self.model = model

        self.controller = controller

        self.mainframe = MainFrame(model, controller)
        # Handle user clicking on the window's "x" button
        self.mainframe.Bind(wx.EVT_CLOSE, self._on_close_window)

        self.accept_source_dialog = None
        self.reject_source_dialog = None

        self.mainframe.Show()
        self.mainframe.show_image_loading_dialog()
        self.model.start_loading_images()

    def _on_close_window(self, event):
        self.close()

    @guithread
    def view_image(self, fits_image):
        self.mainframe.view_image(fits_image)

    @guithread
    def draw_circle(self, x, y, radius):
        self.mainframe.draw_circle(x, y, radius)

    @guithread
    def reset_colormap(self):
        self.mainframe.reset_colormap()

    def close(self):
        self.model.stop_loading_images()
        self.mainframe.Destroy()

    @guithread
    def show_image_loading_dialog(self):
        self.mainframe.show_image_loading_dialog()

    @guithread
    def hide_image_loading_dialog(self):
        self.mainframe.hide_image_loading_dialog()

    @guithread
    def set_source_status(self, current_source, total_sources):
        self.mainframe.set_source_status(current_source, total_sources)

    @guithread
    def set_observation_status(self, current_obs, total_obs):
        self.mainframe.set_observation_status(current_obs, total_obs)

    @guithread
    def set_loading_status(self, loaded, total):
        self.mainframe.set_loading_status(loaded, total)

    @guithread
    def enable_source_validation(self):
        self.mainframe.enable_validation()

    @guithread
    def disable_source_validation(self):
        self.mainframe.disable_validation()

    def is_source_validation_enabled(self):
        return self.mainframe.is_source_validation_enabled()

    def show_accept_source_dialog(self, preset_vals):
        self.accept_source_dialog = AcceptSourceDialog(
            self.mainframe, self.controller, *preset_vals)
        self.accept_source_dialog.ShowModal()

    def close_accept_source_dialog(self):
        if self.accept_source_dialog is not None:
            self.accept_source_dialog.Destroy()

    def show_reject_source_dialog(self):
        self.reject_source_dialog = RejectSourceDialog(
            self.mainframe, self.controller)
        self.reject_source_dialog.ShowModal()

    def close_reject_source_dialog(self):
        if self.reject_source_dialog is not None:
            self.reject_source_dialog.Destroy()

    def all_processed_should_exit_prompt(self):
        return should_exit_prompt(self.mainframe)

    def as_widget(self):
        return self.mainframe


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
        self.menubar = self._create_menus()

        self.main_panel = wx.Panel(self, style=wx.RAISED_BORDER)
        self.control_panel = wx.Panel(self.main_panel)

        self.nav_view = NavPanel(self.control_panel, self.controller)

        self.data_view = self._create_data_notebook()

        self.validation_view = SourceValidationPanel(self.control_panel, self.controller)

        self.viewer_panel = wx.Panel(self.main_panel, style=wx.RAISED_BORDER)
        self.image_viewer = MPLImageViewer(self.viewer_panel)

        self.statusbar = AppStatusBar(self)
        self.SetStatusBar(self.statusbar)

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
        main_sizer.Add(self.viewer_panel, flag=wx.EXPAND)

        self.main_panel.SetSizerAndFit(main_sizer)

    def _create_menus(self):
        def do_bind(handler, item):
            self.Bind(wx.EVT_MENU, handler, item)

        # Create menus and their contents
        file_menu = wx.Menu()
        exit_item = file_menu.Append(wx.ID_EXIT, "Exit", "Exit the program")
        do_bind(self._on_select_exit, exit_item)

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

    def _on_select_exit(self, event):
        self.controller.on_exit()

    def view_image(self, fits_image):
        self.image_viewer.view_image(fits_image)

    def draw_circle(self, x, y, radius):
        self.image_viewer.draw_circle(x, y, radius)

    def reset_colormap(self):
        self.image_viewer.reset_colormap()

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

    def disable_validation(self):
        self.validation_view.disable()

    def enable_validation(self):
        self.validation_view.enable()

    def is_source_validation_enabled(self):
        return self.validation_view.is_validation_enabled()


class KeybindManager(object):
    def __init__(self, view, controller):
        self.controller = controller
        self.view = view

        next_obs_kb_id = wx.NewId()
        prev_obs_kb_id = wx.NewId()
        accept_src_kb_id = wx.NewId()
        reject_src_kb_id = wx.NewId()
        reset_cmap_kb_id = wx.NewId()

        view.Bind(wx.EVT_MENU, self.on_next_obs_keybind, id=next_obs_kb_id)
        view.Bind(wx.EVT_MENU, self.on_prev_obs_keybind, id=prev_obs_kb_id)
        view.Bind(wx.EVT_MENU, self.on_accept_src_keybind, id=accept_src_kb_id)
        view.Bind(wx.EVT_MENU, self.on_reject_src_keybind, id=reject_src_kb_id)
        view.Bind(wx.EVT_MENU, self.on_reset_cmap_keybind, id=reset_cmap_kb_id)

        accept_key = config.read("KEYBINDS.ACCEPT_SRC")
        reject_key = config.read("KEYBINDS.REJECT_SRC")
        reset_cmap_key = config.read("KEYBINDS.RESET_CMAP")

        accelerators = wx.AcceleratorTable(
            [
                (wx.ACCEL_NORMAL, wx.WXK_TAB, next_obs_kb_id),
                (wx.ACCEL_SHIFT, wx.WXK_TAB, prev_obs_kb_id),
                (wx.ACCEL_NORMAL, ord(accept_key), accept_src_kb_id),
                (wx.ACCEL_NORMAL, ord(reject_key), reject_src_kb_id),
                (wx.ACCEL_NORMAL, ord(reset_cmap_key), reset_cmap_kb_id),
            ]
        )

        view.SetAcceleratorTable(accelerators)

    def on_next_obs_keybind(self, event):
        self.controller.on_next_obs()

        # Note: event consumed (no call to event.Skip()) so that we don't
        # have tab iterating over the buttons like it does by default

    def on_prev_obs_keybind(self, event):
        self.controller.on_previous_obs()

    def on_accept_src_keybind(self, event):
        self.controller.on_accept()

    def on_reject_src_keybind(self, event):
        self.controller.on_reject()

    def on_reset_cmap_keybind(self, event):
        self.view.reset_colormap()


class NavPanel(wx.Panel):
    NEXT_LABEL = "Next"
    PREV_LABEL = "Previous"

    def __init__(self, parent, controller):
        super(NavPanel, self).__init__(parent)

        self.controller = controller

        self._init_ui()
        self._bind_events()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="Navigate observations for this source")

        self.status_text = wx.StaticText(self, label="Loading...")
        self.next_button = wx.Button(self, wx.ID_FORWARD, label=self.NEXT_LABEL)
        self.prev_button = wx.Button(self, wx.ID_BACKWARD, label=self.PREV_LABEL)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.status_text, flag=wx.ALIGN_CENTER | wx.TOP, border=10)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.prev_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.next_button, proportion=0, flag=wx.ALL, border=button_border)

        vsizer.Add(hsizer)

        sbox_sizer.Add(vsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)

    def _bind_events(self):
        self.next_button.Bind(wx.EVT_BUTTON, self._on_next)
        self.prev_button.Bind(wx.EVT_BUTTON, self._on_prev)

    def _on_next(self, event):
        self.controller.on_next_obs()

    def _on_prev(self, event):
        self.controller.on_previous_obs()

    def set_status(self, current_obs, total_obs):
        assert 0 <= current_obs <= total_obs
        self.status_text.SetLabel("Observation %s of %s" % (current_obs, total_obs))


class ListCtrlPanel(wx.Panel, listmix.ColumnSorterMixin):
    """
    A list control panel with sortable columns.
    """

    def __init__(self, parent, columns):
        super(ListCtrlPanel, self).__init__(parent)

        self.list = ListCtrlAutoWidth(self,
                                      style=wx.LC_REPORT |
                                            wx.BORDER_NONE |
                                            wx.LC_EDIT_LABELS |
                                            wx.LC_SORT_ASCENDING
        )

        for i, column in enumerate(columns):
            self.list.InsertColumn(i, column)

        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(self.list, 1, wx.EXPAND)
        self.SetSizer(sizer)

    def populate_list(self, datatuples):
        # Clear any existing data
        self.list.DeleteAllItems()

        item_data_map = {}
        index = 0
        for tuple in datatuples:
            self.list.InsertStringItem(index, str(index))
            for colindex, item in enumerate(tuple):
                self.list.SetStringItem(index, colindex, str(item))

            # For ColumnSorterMixin
            self.list.SetItemData(index, index)
            item_data_map[index] = tuple

            index += 1

        self.list.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        self.list.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        # This field is required to be set for the ColumnSorterMixin to work
        self.itemDataMap = item_data_map
        listmix.ColumnSorterMixin.__init__(self, 2)

    def GetListCtrl(self):
        """Required by ColumnSorterMixin"""
        return self.list


class ListCtrlAutoWidth(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin):
    def __init__(self, parent, size=wx.DefaultSize, style=0):
        super(ListCtrlAutoWidth, self).__init__(parent, size=size, style=style)
        listmix.ListCtrlAutoWidthMixin.__init__(self)


class KeyValueListPanel(ListCtrlPanel):
    def __init__(self, parent, get_data):
        """
        Constructor.

        Args:
          parent: wx.Window
            the parent window of this new panel
          get_data: callable
            when called this should provide the data which will populate
            the key-value list. The returned data should be a list of
            (key, value) tuples.
        """
        super(KeyValueListPanel, self).__init__(parent, ("Key", "Value"))

        self.get_data = get_data

        self.display_data()

    def display_data(self):
        self.populate_list(self.get_data())

    def on_change_data(self, event):
        self.display_data()


class SourceValidationPanel(wx.Panel):
    def __init__(self, parent, controller,
                 accept_label="Accept", reject_label="Reject"):
        super(SourceValidationPanel, self).__init__(parent)

        self.controller = controller

        self.accept_label = accept_label
        self.reject_label = reject_label

        self._init_ui()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="Source Validation")

        self.accept_button = wx.Button(self, label=self.accept_label)
        self.reject_button = wx.Button(self, label=self.reject_label)

        self.accept_button.Bind(wx.EVT_BUTTON, self._on_click_accept)
        self.reject_button.Bind(wx.EVT_BUTTON, self._on_click_reject)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.accept_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.reject_button, proportion=0, flag=wx.ALL, border=button_border)

        sbox_sizer.Add(hsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)

    def _on_click_accept(self, event):
        self.controller.on_accept()

    def _on_click_reject(self, event):
        self.controller.on_reject()

    def disable(self):
        self.accept_button.Disable()
        self.reject_button.Disable()

    def enable(self):
        self.accept_button.Enable()
        self.reject_button.Enable()

    def is_validation_enabled(self):
        return self.accept_button.IsEnabled() and self.reject_button.IsEnabled()


class KeyboardCompleteComboBox(wx.ComboBox):
    """
    A combo-box with read-only, preset values.  When the user types a key
    in the text field, it will look through the possible choices and if
    there is one starting with that character, it will select it.
    """

    def __init__(self, parent, choices=None, **kwargs):
        if choices is None:
            choices = []

        self.choices = choices
        super(KeyboardCompleteComboBox, self).__init__(parent, choices=choices,
                                                       style=wx.CB_READONLY,
                                                       **kwargs)
        self.Bind(wx.EVT_CHAR, self._on_char)

    def _on_char(self, event):
        keycode = event.GetKeyCode()

        if keycode == wx.WXK_RETURN:
            event.Skip()
            return

        if keycode < 0 or keycode > 255:
            # Something like a up arrow key, etc.  Ignore it.
            return

        char = chr(keycode)
        for choice in self.choices:
            if choice.startswith(char):
                self.SetStringSelection(choice)


class SourceValidationDialog(wx.Dialog):
    SUBMIT_BTN = "Submit"
    CANCEL_BTN = "Cancel"

    def __init__(self, parent, title=""):
        super(SourceValidationDialog, self).__init__(parent, title=title)

        self._init_ui()

        self.submit_button = wx.Button(
            self, label=self.SUBMIT_BTN, name=SourceValidationDialog.SUBMIT_BTN)
        self.cancel_button = wx.Button(
            self, label=self.CANCEL_BTN, name=SourceValidationDialog.CANCEL_BTN)

        self.submit_button.Bind(wx.EVT_BUTTON, self._on_submit)
        self.cancel_button.Bind(wx.EVT_BUTTON, self._on_cancel)

        self._do_layout()

        self.submit_button.SetDefault()

    def _create_horizontal_pair(self, widget1, widget2, flag=0, border=0):
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(widget1, flag=flag, border=border)
        hsizer.Add(widget2, flag=flag, border=border)
        return hsizer

    def _do_layout(self):
        vsizer = wx.BoxSizer(wx.VERTICAL)
        for widget in self._get_vertical_widget_list():
            vsizer.Add(widget, proportion=0, flag=wx.ALL|wx.EXPAND, border=5)

        vsizer.Add(self._create_horizontal_pair(self.submit_button, self.cancel_button,
                                                flag=wx.ALL, border=5),
                   flag=wx.ALIGN_CENTER)

        # Extra border padding
        bordersizer = wx.BoxSizer(wx.VERTICAL)
        bordersizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(bordersizer)

    def _init_ui(self):
        raise NotImplementedError()

    def _on_submit(self):
        raise NotImplementedError()

    def _on_cancel(self):
        raise NotImplementedError()

    def _get_vertical_widget_list(self):
        raise NotImplementedError()


class AcceptSourceDialog(SourceValidationDialog):
    TITLE = "Accept Source"
    MINOR_PLANET_NUMBER = "Minor planet number: "
    PROVISIONAL_NAME = "Provisional name: "
    DISCOVERY_ASTERISK = "Discovery asterisk: "
    NOTE1 = "Note 1: "
    NOTE2 = "Note 2: "
    DATE_OF_OBS = "Date of observation: "
    RA = "Right ascension: "
    DEC = "Declination: "
    OBS_MAG = "Observed magnitude: "
    BAND = "Band: "
    OBSERVATORY_CODE = "Observatory code: "
    COMMENT = "Comment: "

    def __init__(self, parent, controller, provisional_name, already_discovered, date_of_obs, ra, dec, obs_mag, band,
                 note1_choices=None, note2_choices=None, note2_default=None, default_observatory_code=""):

        self.controller = controller
        self.provisional_name = provisional_name
        self.already_discovered = already_discovered
        self.date_of_obs = date_of_obs
        self.ra_str = str(ra)
        self.dec_str = str(dec)
        self.obs_mag = str(obs_mag)
        self.band = band
        self.default_observatory_code = str(default_observatory_code)

        self.note1_choices = note1_choices if note1_choices is not None else []
        self.note2_choices = note2_choices if note2_choices is not None else []

        self.note2_default = note2_default if note2_default is not None else ""

        super(AcceptSourceDialog, self).__init__(parent, title=self.TITLE)

        self.note1_combobox.SetFocus()

    def _init_ui(self):
        self.minor_planet_num_label = wx.StaticText(
            self, label=AcceptSourceDialog.MINOR_PLANET_NUMBER)
        self.minor_planet_num_text = wx.TextCtrl(
            self, name=AcceptSourceDialog.MINOR_PLANET_NUMBER)

        self.provisional_name_label = wx.StaticText(
            self, label=AcceptSourceDialog.PROVISIONAL_NAME)
        self.provision_name_text = wx.StaticText(
            self, label=self.provisional_name, name=self.PROVISIONAL_NAME)

        self.discovery_asterisk_label = wx.StaticText(
            self, label=AcceptSourceDialog.DISCOVERY_ASTERISK)
        discovery_asterisk = "No" if self.already_discovered else "Yes"
        self.discovery_asterisk_text = wx.StaticText(self, label=discovery_asterisk)

        self.note1_label = wx.StaticText(self, label=AcceptSourceDialog.NOTE1)
        self.note1_combobox = KeyboardCompleteComboBox(
            self, choices=self.note1_choices, name=AcceptSourceDialog.NOTE1)

        self.note2_label = wx.StaticText(self, label=AcceptSourceDialog.NOTE2)
        self.note2_combobox = KeyboardCompleteComboBox(
            self, value=self.note2_default, choices=self.note2_choices,
            name=AcceptSourceDialog.NOTE2)

        self.date_of_obs_label = wx.StaticText(
            self, label=AcceptSourceDialog.DATE_OF_OBS)
        self.date_of_obs_text = wx.StaticText(
            self, label=self.date_of_obs, name=AcceptSourceDialog.DATE_OF_OBS)

        self.ra_label = wx.StaticText(self, label=AcceptSourceDialog.RA)
        self.ra_text = wx.StaticText(
            self, label=self.ra_str, name=AcceptSourceDialog.RA)

        self.dec_label = wx.StaticText(self, label=AcceptSourceDialog.DEC)
        self.dec_text = wx.StaticText(
            self, label=self.dec_str, name=AcceptSourceDialog.DEC)

        self.obs_mag_label = wx.StaticText(self, label=AcceptSourceDialog.OBS_MAG)
        self.obs_mag_text = wx.StaticText(
            self, label=self.obs_mag, name=self.OBS_MAG)

        self.band_label = wx.StaticText(self, label=AcceptSourceDialog.BAND)
        self.band_text = wx.StaticText(
            self, label=self.band, name=AcceptSourceDialog.BAND)

        self.observatory_code_label = wx.StaticText(
            self, label=AcceptSourceDialog.OBSERVATORY_CODE)
        self.observatory_code_text = wx.TextCtrl(
            self, name=AcceptSourceDialog.OBSERVATORY_CODE)
        self.observatory_code_text.SetValue(self.default_observatory_code)

        self.comment_label = wx.StaticText(self, label=AcceptSourceDialog.COMMENT)
        self.comment_text = wx.TextCtrl(self, name=AcceptSourceDialog.COMMENT,
                                        style=wx.TE_MULTILINE)

    def _get_vertical_widget_list(self):
        comment_sizer = wx.BoxSizer(wx.VERTICAL)
        comment_sizer.Add(self.comment_label, flag=wx.ALIGN_CENTER)
        comment_sizer.Add(self.comment_text, flag=wx.EXPAND)

        return [self._create_horizontal_pair(self.minor_planet_num_label, self.minor_planet_num_text),
                self._create_horizontal_pair(self.provisional_name_label, self.provision_name_text),
                self._create_horizontal_pair(self.discovery_asterisk_label, self.discovery_asterisk_text),
                (0, 0), # blank space
                self._create_horizontal_pair(self.note1_label, self.note1_combobox),
                self._create_horizontal_pair(self.note2_label, self.note2_combobox),
                (0, 0), # blank space
                self._create_horizontal_pair(self.date_of_obs_label, self.date_of_obs_text),
                self._create_horizontal_pair(self.ra_label, self.ra_text),
                self._create_horizontal_pair(self.dec_label, self.dec_text),
                self._create_horizontal_pair(self.obs_mag_label, self.obs_mag_text),
                self._create_horizontal_pair(self.band_label, self.band_text),
                self._create_horizontal_pair(self.observatory_code_label, self.observatory_code_text),
                comment_sizer,
                (0, 0)  # blank space
        ]

    def _on_submit(self, event):
        # Grab data out of the form
        # TODO validation
        minor_planet_number = self.minor_planet_num_text.GetValue()
        discovery_asterisk = " " if self.already_discovered else "*"
        note1 = self.note1_combobox.GetValue()
        note2 = self.note2_combobox.GetValue()
        obs_mag = self.obs_mag
        band = self.band
        observatory_code = self.observatory_code_text.GetValue()
        comment = self.comment_text.GetValue()

        self.controller.on_do_accept(minor_planet_number,
                                     self.provisional_name,
                                     discovery_asterisk,
                                     note1,
                                     note2,
                                     self.date_of_obs,
                                     self.ra_str,
                                     self.dec_str,
                                     obs_mag,
                                     band,
                                     observatory_code,
                                     comment
        )

    def _on_cancel(self, event):
        self.controller.on_cancel_accept()


class RejectSourceDialog(SourceValidationDialog):
    TITLE = "Reject Source"
    COMMENT = "Comment: "

    def __init__(self, parent, controller):
        super(RejectSourceDialog, self).__init__(parent, title=self.TITLE)
        self.controller = controller
        self.comment_text.SetFocus()

    def _init_ui(self):
        self.comment_label = wx.StaticText(self, label=RejectSourceDialog.COMMENT)
        self.comment_text = wx.TextCtrl(self, name=RejectSourceDialog.COMMENT,
                                        style=wx.TE_MULTILINE)

    def _get_vertical_widget_list(self):
        comment_sizer = wx.BoxSizer(wx.VERTICAL)
        comment_sizer.Add(self.comment_label, flag=wx.ALIGN_CENTER)
        comment_sizer.Add(self.comment_text, flag=wx.EXPAND)

        return [self._create_horizontal_pair(self.comment_label, self.comment_text),
                comment_sizer,
                (0, 0)  # blank space
               ]

    def _on_submit(self, event):
        comment = self.comment_text.GetValue()
        self.controller.on_do_reject(comment)

    def _on_cancel(self, event):
        self.controller.on_cancel_reject()


class AppStatusBar(wx.StatusBar):
    LOADING_MSG = "Loading..."

    def __init__(self, parent):
        super(AppStatusBar, self).__init__(parent)

        self.SetFieldsCount(2)
        self.SetStatusText(self.LOADING_MSG, 0)
        self.SetStatusText(self.LOADING_MSG, 1)

    def set_source_status(self, current_source, total_sources):
        self.SetStatusText("Source %s of %s" % (current_source, total_sources), 0)

    def get_source_status(self):
        return self.GetStatusText(0)

    def set_loading_status(self, loaded, total):
        self.SetStatusText("Loaded %s of %s images" % (loaded, total), 1)

    def get_loading_status(self):
        return self.GetStatusText(1)


class WaitingGaugeDialog(wx.Dialog):
    def __init__(self, parent, wait_message, pulse_period_ms=100):
        super(WaitingGaugeDialog, self).__init__(parent)

        self.wait_message = wait_message
        self.pulse_period_ms = pulse_period_ms

        self._init_ui()

    def _init_ui(self):
        # Non-visible component used to periodically update gauge
        self.timer = wx.Timer(self)

        # Create visible components
        self.msg = wx.StaticText(self, label=self.wait_message)
        self.gauge = wx.Gauge(self)
        self.hidebutton = wx.Button(self, label="Hide")

        self._do_layout()

        self.hidebutton.Bind(wx.EVT_BUTTON, self._on_hide)
        self.Bind(wx.EVT_TIMER, self._on_tick, self.timer)

        self.timer.Start(self.pulse_period_ms)

    def _do_layout(self):
        vborder = 10
        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.msg, flag=wx.CENTER | wx.TOP | wx.BOTTOM, border=vborder)
        vsizer.Add(self.gauge, flag=wx.EXPAND | wx.TOP | wx.BOTTOM, border=vborder)
        vsizer.Add(self.hidebutton, flag=wx.CENTER | wx.TOP | wx.BOTTOM, border=vborder)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(vsizer, flag=wx.LEFT | wx.RIGHT, border=20)

        self.SetSizer(hsizer)
        hsizer.Fit(self)

    def _on_tick(self, event):
        self.gauge.Pulse()

    def _on_hide(self, event):
        self.Hide()


def should_exit_prompt(parent):
    dialog = wx.MessageDialog(parent,
                              config.read("UI.ALLPROC.MSG"),
                              caption=config.read("UI.ALLPROC.CAPTION"),
                              style=wx.YES_NO | wx.ICON_INFORMATION)

    user_choice = dialog.ShowModal()
    dialog.Destroy()

    return True if user_choice == wx.ID_YES else False


def get_asset_full_path(asset_name):
    """Get the full path of an asset based on its filename"""
    return os.path.join(os.path.dirname(__file__), "assets", asset_name)

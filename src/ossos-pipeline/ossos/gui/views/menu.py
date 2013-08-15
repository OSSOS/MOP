__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui.views.listctrls import ListCtrlPanel


class Menu(object):
    def __init__(self, frame, controller):
        self.controller = controller
        self.frame = frame

        self._build_menu()

    def _build_menu(self):
        def do_bind(handler, item):
            self.frame.Bind(wx.EVT_MENU, handler, item)

        # Create menus and their contents
        file_menu = wx.Menu()
        keymap_item = file_menu.Append(
            id=wx.ID_ANY,
            text="Keymap",
            help="Show mappings for keyboard shortcuts.")
        exit_item = file_menu.Append(
            id=wx.ID_EXIT,
            text="Exit",
            help="Exit the program")

        do_bind(self._on_select_keymap, keymap_item)
        do_bind(self._on_select_exit, exit_item)

        display_menu = wx.Menu()
        singlet_view_item = display_menu.Append(
            id=wx.ID_ANY,
            text="Single images",
            kind=wx.ITEM_RADIO)
        triplet_view_item = display_menu.Append(
            id=wx.ID_ANY,
            text="Image triplets",
            kind=wx.ITEM_RADIO)
        display_menu.AppendSeparator()
        auto_play_item = display_menu.Append(
            id=wx.ID_ANY,
            text="Autoplay",
            help="Automatically transition through images.",
            kind=wx.ITEM_CHECK)

        do_bind(self._on_select_singlet_view, singlet_view_item)
        do_bind(self._on_select_triplet_view, triplet_view_item)
        do_bind(self._on_select_autoplay, auto_play_item)

        sync_menu = wx.Menu()
        auto_sync_item = sync_menu.Append(
            id=wx.ID_ANY,
            text="Automatically",
            help="Automatically synchronize results.",
            kind=wx.ITEM_CHECK)

        do_bind(self._on_select_automatically_sync, auto_sync_item)

        # Create menu bar
        menubar = wx.MenuBar()
        self.file_menu_title = "File"
        menubar.Append(file_menu, self.file_menu_title)
        self.display_menu_title = "Display"
        menubar.Append(display_menu, self.display_menu_title)
        self.sync_menu_title = "Sync"
        menubar.Append(sync_menu, self.sync_menu_title)

        self.frame.SetMenuBar(menubar)

        self.menubar = menubar
        self.auto_play_item = auto_play_item

        triplet_view_item.Enable(False)

    def _on_select_keymap(self, event):
        dialog = wx.Dialog(self.frame, title="Key Mappings")
        panel = ListCtrlPanel(dialog, ("Action", "Shortcut"))
        panel.populate_list(self.frame.keybind_manager.get_keymappings())

        dialog.Show()

    def _on_select_exit(self, event):
        self.controller.on_exit()

    def _on_select_automatically_sync(self, event):
        if event.Checked():
            self.controller.on_enable_auto_sync()
        else:
            self.controller.on_disable_auto_sync()

    def _on_select_singlet_view(self, event):
        self.controller.on_use_singlet_view()

    def _on_select_triplet_view(self, event):
        self.controller.on_use_triplet_view()

    def _on_select_autoplay(self, event):
        if event.Checked():
            self.controller.on_enable_autoplay()
        else:
            self.controller.on_disable_autoplay()

    def disable_menu_item(self, item_title):
        self.menubar.EnableTop(
            self.menubar.FindMenu(item_title), False)

    def disable_sync(self):
        self.disable_menu_item(self.sync_menu_title)

    def set_autoplay(self, autoplay_enabled):
        self.auto_play_item.Check(autoplay_enabled)
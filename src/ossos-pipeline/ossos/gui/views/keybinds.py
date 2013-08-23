__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import config


class KeybindManager(object):
    def __init__(self, view, controller):
        self.controller = controller
        self.view = view

        next_obs_kb_id = wx.NewId()
        prev_obs_kb_id = wx.NewId()
        accept_src_kb_id = wx.NewId()
        reject_src_kb_id = wx.NewId()
        reset_cmap_kb_id = wx.NewId()
        reset_src_kb_id = wx.NewId()
        autoplay_kb_id = wx.NewId()
        toggle_reticule_kb_id = wx.NewId()

        def bind(handler, kb_id):
            view.Bind(wx.EVT_MENU, handler, id=kb_id)

        bind(self.on_next_obs_keybind, next_obs_kb_id)
        bind(self.on_prev_obs_keybind, prev_obs_kb_id)
        bind(self.on_accept_src_keybind, accept_src_kb_id)
        bind(self.on_reject_src_keybind, reject_src_kb_id)
        bind(self.on_reset_cmap_keybind, reset_cmap_kb_id)
        bind(self.on_reset_source_location_keybind, reset_src_kb_id)
        bind(self.on_toggle_autoplay, autoplay_kb_id)
        bind(self.on_toggle_reticule, toggle_reticule_kb_id)

        self.accept_key = config.read("KEYBINDS.ACCEPT_SRC")
        self.reject_key = config.read("KEYBINDS.REJECT_SRC")
        self.reset_cmap_key = config.read("KEYBINDS.RESET_CMAP")
        self.reset_source_key = config.read("KEYBINDS.RESET_SOURCE_LOCATION")
        self.autoplay_key = config.read("KEYBINDS.AUTOPLAY")
        self.toggle_reticule_key = config.read("KEYBINDS.TOGGLE_RETICULE")

        accelerators = wx.AcceleratorTable(
            [
                (wx.ACCEL_NORMAL, wx.WXK_TAB, next_obs_kb_id),
                (wx.ACCEL_SHIFT, wx.WXK_TAB, prev_obs_kb_id),
                (wx.ACCEL_NORMAL, ord(self.accept_key), accept_src_kb_id),
                (wx.ACCEL_NORMAL, ord(self.reject_key), reject_src_kb_id),
                (wx.ACCEL_NORMAL, ord(self.reset_cmap_key), reset_cmap_kb_id),
                (wx.ACCEL_NORMAL, ord(self.reset_source_key), reset_src_kb_id),
                (wx.ACCEL_NORMAL, ord(self.autoplay_key), autoplay_kb_id),
                (wx.ACCEL_NORMAL, ord(self.toggle_reticule_key),
                 toggle_reticule_kb_id),
            ]
        )

        view.SetAcceleratorTable(accelerators)

    def get_keymappings(self):
        return [("Next observation", "Tab"),
                ("Previous observation", "Shift + Tab"),
                ("Accept", self.accept_key),
                ("Reject", self.reject_key),
                ("Reset colourmap", self.reset_cmap_key),
                ("Reset source location", self.reset_source_key),
                ("Autoplay", self.autoplay_key),
                ("Toggle reticule", self.toggle_reticule_key)]

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
        self.controller.on_reset_colormap()

    def on_reset_source_location_keybind(self, event):
        self.controller.on_reset_source_location()

    def on_toggle_autoplay(self, event):
        self.controller.on_toggle_autoplay_key()

    def on_toggle_reticule(self, event):
        self.controller.on_toggle_reticule_key()

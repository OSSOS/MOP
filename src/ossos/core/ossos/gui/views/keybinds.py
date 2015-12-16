__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import config, logger


class KeybindManager(object):
    def __init__(self, view, controller):
        """

        :param view:
        :param controller: the controller to bind this key to
        :type controller: AbstractController
        :return:
        """
        logger.debug("Building KeybindManager.")
        self.controller = controller
        self.view = view

        next_obs_kb_id = wx.NewId()
        prev_obs_kb_id = wx.NewId()
        accept_src_kb_id = wx.NewId()
        auto_accept_src_kd_id = wx.NewId()
        reject_src_kb_id = wx.NewId()
        reset_cmap_kb_id = wx.NewId()
        reset_src_kb_id = wx.NewId()
        autoplay_kb_id = wx.NewId()
        load_comparison_kb_id = wx.NewId()
        load_diff_comparison_kb_id = wx.NewId()
        toggle_reticule_kb_id = wx.NewId()
        toggle_align_kb_id = wx.NewId()

        def bind(handler, kb_id):
            view.Bind(wx.EVT_MENU, handler, id=kb_id)

        bind(self.on_load_comparison_keybind, load_comparison_kb_id)
        bind(self.on_load_diff_comparison_keybind, load_diff_comparison_kb_id)
        bind(self.on_next_obs_keybind, next_obs_kb_id)
        bind(self.on_prev_obs_keybind, prev_obs_kb_id)
        bind(self.on_accept_src_keybind, accept_src_kb_id)
        bind(self.on_auto_accept_src_keybind, auto_accept_src_kd_id)
        bind(self.on_reject_src_keybind, reject_src_kb_id)
        bind(self.on_reset_cmap_keybind, reset_cmap_kb_id)
        bind(self.on_reset_source_location_keybind, reset_src_kb_id)
        bind(self.on_toggle_autoplay, autoplay_kb_id)
        bind(self.on_toggle_reticule, toggle_reticule_kb_id)
        bind(self.on_toggle_align, toggle_align_kb_id)

        self.toggle_align_key = config.read("KEYBINDS.TOGGLE_ALIGN")
        self.accept_key = config.read("KEYBINDS.ACCEPT_SRC")
        self.auto_accept_key = config.read("KEYBINDS.AUTO_ACCEPT_SRC")
        self.reject_key = config.read("KEYBINDS.REJECT_SRC")
        self.reset_cmap_key = config.read("KEYBINDS.RESET_CMAP")
        self.reset_source_key = config.read("KEYBINDS.RESET_SOURCE_LOCATION")
        self.autoplay_key = config.read("KEYBINDS.AUTOPLAY")
        self.toggle_reticule_key = config.read("KEYBINDS.TOGGLE_RETICULE")
        self.load_comparison_key = config.read("KEYBINDS.LOAD_COMPARISON")
        self.load_diff_comparison_key = config.read("KEYBINDS.LOAD_DIFF_COMPARISON")

        accelerators = [
            (wx.ACCEL_NORMAL, wx.WXK_TAB, next_obs_kb_id),
            (wx.ACCEL_SHIFT, wx.WXK_TAB, prev_obs_kb_id),
            (wx.ACCEL_NORMAL, ord(self.accept_key), accept_src_kb_id),
            (wx.ACCEL_NORMAL, ord(self.auto_accept_key), auto_accept_src_kd_id),
            (wx.ACCEL_NORMAL, ord(self.reject_key), reject_src_kb_id),
            (wx.ACCEL_NORMAL, ord(self.reset_cmap_key), reset_cmap_kb_id),
            (wx.ACCEL_NORMAL, ord(self.reset_source_key), reset_src_kb_id),
            (wx.ACCEL_NORMAL, ord(self.load_comparison_key), load_comparison_kb_id),
            (wx.ACCEL_NORMAL, ord(self.load_diff_comparison_key), load_diff_comparison_kb_id),
            (wx.ACCEL_NORMAL, ord(self.autoplay_key), autoplay_kb_id),
            (wx.ACCEL_NORMAL, ord(self.toggle_reticule_key), toggle_reticule_kb_id),
            (wx.ACCEL_NORMAL, ord(self.toggle_align_key), toggle_align_kb_id),
        ]

        for line in config.read("MPC.NOTE1OPTIONS"):
            if not len(line) > 0:
                continue
            this_key = line.split()[0][0].lower()
            this_func = self._build_accept_func(this_key)
            this_key_id = wx.NewId()
            bind(this_func, this_key_id)
            accelerators.append((wx.ACCEL_CTRL, ord(this_key), this_key_id))

        accelerators = wx.AcceleratorTable(accelerators)
        view.SetAcceleratorTable(accelerators)

    def _build_accept_func(self, key):
        def func(event):
            self.controller.on_accept(auto=key)
        return func

    def get_keymappings(self):
        return [("Next observation", "Tab"),
                ("Previous observation", "Shift + Tab"),
                ("Accept", self.accept_key),
                ("Auto Accept", self.auto_accept_key),
                ("Reject", self.reject_key),
                ("Reset colourmap", self.reset_cmap_key),
                ("Reset source location", self.reset_source_key),
                ("Autoplay", self.autoplay_key),
                ("Load Comparison Image", self.load_comparison_key),
                ("Load Difference Comaprison", self.load_diff_comparison_key),
                ("Toggle reticule", self.toggle_reticule_key),
                ("Toggle focus", self.toggle_align_key)]

    def on_load_diff_comparison_keybind(self, event):
        logger.debug("BBB!")
        self.controller.on_load_comparison(research=True)

    def on_load_comparison_keybind(self, event):
        self.controller.on_load_comparison()

    def on_next_obs_keybind(self, event):
        self.controller.on_next_obs()

        # Note: event consumed (no call to event.Skip()) so that we don't
        # have tab iterating over the buttons like it does by default

    def on_prev_obs_keybind(self, event):
        self.controller.on_previous_obs()

    def on_auto_accept_src_keybind(self, event):
        self.controller.on_accept(auto=True)

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

    def on_toggle_align(self, event):
        self.controller.on_toggle_align()

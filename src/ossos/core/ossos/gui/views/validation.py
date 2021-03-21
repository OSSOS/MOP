__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class MPCPanel(wx.Panel):
    def __init__(self, parent, controller,
                 ssos_label="Query", save_label="Save"):
        super(MPCPanel, self).__init__(parent)

        self.controller = controller

        self.ssos_label = ssos_label
        self.save_label = save_label

        self._init_ui()

    def _init_ui(self):
        self.sbox = wx.StaticBox(self, label="MPC Builder")

        self.ssos_button = wx.Button(self, label=self.ssos_label)
        self.save_button = wx.Button(self, label=self.save_label)

        self.ssos_button.Bind(wx.EVT_BUTTON, self._on_click_ssos)
        self.save_button.Bind(wx.EVT_BUTTON, self._on_click_save)

        self._do_layout()

    def _do_layout(self):
        sbox_sizer = wx.StaticBoxSizer(self.sbox, wx.VERTICAL)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        button_border = 10
        hsizer.Add(self.save_button, proportion=0, flag=wx.ALL, border=button_border)
        hsizer.Add(self.ssos_button, proportion=0, flag=wx.ALL, border=button_border)

        sbox_sizer.Add(hsizer, flag=wx.ALIGN_CENTER)

        # Add a bit of border around the box sizer
        border_sizer = wx.BoxSizer(wx.VERTICAL)
        border_sizer.Add(sbox_sizer, flag=wx.EXPAND | wx.ALL, border=10)

        self.SetSizer(border_sizer)

    def _on_click_ssos(self, event):
        self.controller.on_ssos()

    def _on_click_save(self, event):
        self.controller.on_save()

    def disable(self):
        self.save_button.Disable()
        self.ssos_button.Disable()

    def enable(self):
        self.save_button.Enable()
        self.ssos_button.Enable()

    def is_validation_enabled(self):
        return self.save_button.IsEnabled() and self.ssos_button.IsEnabled()


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


class SourceValidationDialog(wx.Dialog):
    COMMENT = "Comment: "
    SUBMIT_BTN = "Submit"
    CANCEL_BTN = "Cancel"

    def __init__(self, parent, title=""):
        super(SourceValidationDialog, self).__init__(parent, title=title)

        self._init_ui()

        self.comment_label = wx.StaticText(self, label=SourceValidationDialog.COMMENT)
        self.comment_text = wx.TextCtrl(self, name=SourceValidationDialog.COMMENT)

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
            vsizer.Add(widget, proportion=0, flag=wx.ALL | wx.EXPAND, border=5)

        comment_sizer = wx.BoxSizer(wx.VERTICAL)
        comment_sizer.Add(self.comment_label, flag=wx.ALIGN_CENTER)
        comment_sizer.Add(self.comment_text, flag=wx.EXPAND)

        vsizer.Add(comment_sizer, flag=wx.ALL | wx.EXPAND, border=5)
        vsizer.Add(self._create_horizontal_pair(self.submit_button, self.cancel_button,
                                                flag=wx.ALL, border=5),
                   flag=wx.ALIGN_CENTER)

        # Extra border padding
        bordersizer = wx.BoxSizer(wx.VERTICAL)
        bordersizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(bordersizer)

    def _on_enter_comment(self, event):
        # Don't want it to actually put a newline character, just submit
        # the form.
        self._on_submit(event)

    def _init_ui(self):
        raise NotImplementedError()

    def _on_submit(self, event):
        raise NotImplementedError()

    def _on_cancel(self, event):
        raise NotImplementedError()

    def _get_vertical_widget_list(self):
        raise NotImplementedError()


class AcceptSourceDialog(SourceValidationDialog):
    TITLE = "Accept Source"
    MINOR_PLANET_NUMBER = "Minor planet number: "
    PROVISIONAL_NAME = "Provisional name: "
    NOTE1 = "Note 1: "
    NOTE2 = "Note 2: "
    DATE_OF_OBS = "Date of observation: "
    RA = "Right ascension: "
    DEC = "Declination: "
    OBS_MAG = "Observed magnitude: "
    BAND = "Band: "
    OBSERVATORY_CODE = "Observatory code: "

    def __init__(self, parent, controller,
                 provisional_name,
                 date_of_obs,
                 ra,
                 dec,
                 obs_mag,
                 obs_mag_err,
                 band,
                 note1_choices=None,
                 note2_choices=None,
                 note1_default=None,
                 note2_default=None,
                 default_observatory_code="",
                 default_comment="",
                 phot_failure=False,
                 pixel_x=None,
                 pixel_y=None):
        self.controller = controller
        self.phot_failure = phot_failure

        self.provisional_name = provisional_name
        self.date_of_obs = date_of_obs
        self.ra = ra
        self.dec = dec
        self.ra_str = "{:>14} {:>5.2f}".format(ra, float(pixel_x))
        self.dec_str = "{:>14} {:>5.2f}".format(dec, float(pixel_y))
        if self.phot_failure:
            message = "Photometry failed.  Will be left blank."
            self.obs_mag = message
            self.band = message
            self.obs_mag_err = message
        else:
            self.obs_mag = "{:5.2f}".format(obs_mag)
            self.obs_mag_err = "{:5.2f}".format(obs_mag_err)
            self.band = "{}".format(band)

        self.default_observatory_code = str(default_observatory_code)

        self.note1_choices = note1_choices if note1_choices is not None else []
        self.note2_choices = note2_choices if note2_choices is not None else []

        self.note1_default = note1_default if note1_default is not None else ""
        self.note2_default = note2_default if note2_default is not None else ""

        self.default_comment = default_comment

        super(AcceptSourceDialog, self).__init__(parent, title=self.TITLE)

        self.note1_combobox.SetFocus()

    def _init_ui(self):
        self.minor_planet_num_label = wx.StaticText(
            self, label=AcceptSourceDialog.MINOR_PLANET_NUMBER)
        self.minor_planet_num_text = wx.TextCtrl(
            self, name=AcceptSourceDialog.MINOR_PLANET_NUMBER)

        self.provisional_name_label = wx.StaticText(
            self, label=AcceptSourceDialog.PROVISIONAL_NAME)
        self.provision_name_text = self._create_readonly_text(
            value=self.provisional_name, name=self.PROVISIONAL_NAME)

        self.note1_label = wx.StaticText(self, label=AcceptSourceDialog.NOTE1)
        self.note1_combobox = KeyboardCompleteComboBox(
            self, value=self.note1_default, choices=self.note1_choices,
            name=AcceptSourceDialog.NOTE1)

        self.note2_label = wx.StaticText(self, label=AcceptSourceDialog.NOTE2)
        self.note2_combobox = KeyboardCompleteComboBox(
            self, value=self.note2_default, choices=self.note2_choices,
            name=AcceptSourceDialog.NOTE2)

        self.date_of_obs_label = wx.StaticText(
            self, label=AcceptSourceDialog.DATE_OF_OBS)
        self.date_of_obs_text = self._create_readonly_text(
            value=self.date_of_obs, name=AcceptSourceDialog.DATE_OF_OBS)

        self.ra_label = wx.StaticText(self, label=AcceptSourceDialog.RA)
        self.ra_text = self._create_readonly_text(
            value=self.ra_str, name=AcceptSourceDialog.RA)

        self.dec_label = wx.StaticText(self, label=AcceptSourceDialog.DEC)
        self.dec_text = self._create_readonly_text(
            value=self.dec_str, name=AcceptSourceDialog.DEC)

        self.obs_mag_label = wx.StaticText(self, label=AcceptSourceDialog.OBS_MAG)
        self.obs_mag_text = self._create_readonly_text(
            value=self.obs_mag, name=self.OBS_MAG)

        self.band_label = wx.StaticText(self, label=AcceptSourceDialog.BAND)
        self.band_text = self._create_readonly_text(
            value=self.band, name=AcceptSourceDialog.BAND)

        self.observatory_code_label = wx.StaticText(
            self, label=AcceptSourceDialog.OBSERVATORY_CODE)
        self.observatory_code_text = wx.TextCtrl(
            self, value=self.default_observatory_code,
            name=AcceptSourceDialog.OBSERVATORY_CODE)

    def _create_readonly_text(self, value, name):
        text_ctrl = wx.TextCtrl(self, style=wx.TE_READONLY, value=value,
                                name=name)
        text_ctrl.SetBackgroundColour(self.GetBackgroundColour())
        return text_ctrl

    def _get_vertical_widget_list(self):
        data_fields = [(self.minor_planet_num_label, self.minor_planet_num_text),
                       (self.provisional_name_label, self.provision_name_text),
                       (self.note1_label, self.note1_combobox),
                       (self.note2_label, self.note2_combobox),
                       (self.date_of_obs_label, self.date_of_obs_text),
                       (self.ra_label, self.ra_text),
                       (self.dec_label, self.dec_text),
                       (self.obs_mag_label, self.obs_mag_text),
                       (self.band_label, self.band_text),
                       (self.observatory_code_label, self.observatory_code_text),
                       ]

        sizer = wx.FlexGridSizer(rows=len(data_fields), cols=2)
        for label, value in data_fields:
            sizer.Add(label)
            sizer.Add(value, proportion=1, flag=wx.EXPAND)

        sizer.AddGrowableCol(1, proportion=1)

        return [sizer]

    def _on_submit(self, event):
        # Grab data out of the form
        # TODO validation
        minor_planet_number = self.minor_planet_num_text.GetValue()
        note1 = self.note1_combobox.GetValue()
        note2 = self.note2_combobox.GetValue()
        obs_mag = self.obs_mag if not self.phot_failure else ""
        obs_mag_err = self.obs_mag_err if not self.phot_failure else -1
        band = self.band if not self.phot_failure else ""
        observatory_code = self.observatory_code_text.GetValue()
        comment = self.comment_text.GetValue()

        self.controller.on_do_accept(minor_planet_number,
                                     self.provisional_name,
                                     note1,
                                     note2,
                                     self.date_of_obs,
                                     self.ra,
                                     self.dec,
                                     obs_mag,
                                     obs_mag_err,
                                     band,
                                     observatory_code,
                                     comment)

    def _on_cancel(self, event):
        self.controller.on_cancel_accept()


class OffsetSourceDialog(SourceValidationDialog):
    TITLE = "Accept Re-centroid Dialog"

    def __init__(self, parent, controller, pix_coords=(0, 0), cen_coords=(0, 0)):
        self.cen_coords = cen_coords
        self.pix_coords = pix_coords
        self.default_comment = "DAOphot centroid differs from input value.\n\n"
        self.default_comment += "{:8s} {:6.2f} {:6.2f}\n".format("mark", self.pix_coords.ra, self.pix_coords.dec)
        self.default_comment += "{:8s} {:6.2f} {:6.2f}\n".format("daophot", self.cen_coords.ra, self.cen_coords.dec)
        self.default_comment += "\nAccepted DAOphot centroid or Mark centroid?"

        super(OffsetSourceDialog, self).__init__(parent, title=self.TITLE)
        self.controller = controller

        self.submit_button.SetLabel("DAOPhot")
        self.cancel_button.SetLabel("Marker")

    def _init_ui(self):
        pass

    def _get_vertical_widget_list(self):
        return []

    def _on_submit(self, event):
        self.controller.on_do_offset(self.cen_coords)

    def _on_cancel(self, event):
        self.controller.on_cancel_offset(self.pix_coords)


class RejectSourceDialog(SourceValidationDialog):
    TITLE = "Reject Source"

    def __init__(self, parent, controller, default_comment=""):
        self.default_comment = default_comment

        super(RejectSourceDialog, self).__init__(parent, title=self.TITLE)
        self.controller = controller
        self.comment_text.SetFocus()

    def _init_ui(self):
        pass

    def _get_vertical_widget_list(self):
        return []

    def _on_submit(self, event):
        comment = self.comment_text.GetValue()
        self.controller.on_do_reject(comment)

    def _on_cancel(self, event):
        self.controller.on_cancel_reject()


class VettingSourceDialog(RejectSourceDialog):
    TITLE = "Vetting Source Accept"

    def _on_submit(self, event):
        self.controller.on_do_accept(self.comment_text.GetValue())

    def _on_cancel(self, event):
        self.controller.on_cancel_accept()


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
        super(KeyboardCompleteComboBox, self).__init__(parent,
                                                       choices=choices,
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

__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class AcceptSourceDialog(wx.Dialog):
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
    OK_BTN = "Ok"
    CANCEL_BTN = "Cancel"

    def __init__(self, parent, controller, provisional_name, date_of_obs, ra, dec, obs_mag, band,
                 note1_choices=None, note2_choices=None, note2_default=None, default_observatory_code=""):
        super(AcceptSourceDialog, self).__init__(parent, title=self.TITLE)

        self.controller = controller
        self.provisional_name = provisional_name
        self.date_of_obs = date_of_obs
        self.ra_str = str(ra)
        self.dec_str = str(dec)
        self.obs_mag = str(obs_mag)
        self.band = band
        self.default_observatory_code = str(default_observatory_code)

        self.note1_choices = note1_choices if note1_choices is not None else []
        self.note2_choices = note2_choices if note2_choices is not None else []

        self.note2_default = note2_default if note2_default is not None else ""

        self._init_ui()
        self._bind_events()

    def _init_ui(self):
        self.minor_planet_num_label = wx.StaticText(self, label=self.MINOR_PLANET_NUMBER)
        self.minor_planet_num_text = wx.TextCtrl(self, name=self.MINOR_PLANET_NUMBER)

        self.provisional_name_label = wx.StaticText(self, label=self.PROVISIONAL_NAME)
        self.provision_name_text = wx.StaticText(self, label=self.provisional_name, name=self.PROVISIONAL_NAME)

        self.discovery_asterisk_cb = wx.CheckBox(self, label=self.DISCOVERY_ASTERISK,
                                                 style=wx.ALIGN_RIGHT, name=self.DISCOVERY_ASTERISK)

        self.note1_label = wx.StaticText(self, label=self.NOTE1)
        self.note1_combobox = wx.ComboBox(self, choices=self.note1_choices, style=wx.CB_READONLY,
                                          name=self.NOTE1)

        self.note2_label = wx.StaticText(self, label=self.NOTE2)
        self.note2_combobox = wx.ComboBox(self, value=self.note2_default, choices=self.note2_choices,
                                          style=wx.CB_READONLY, name=self.NOTE2)

        self.date_of_obs_label = wx.StaticText(self, label=self.DATE_OF_OBS)
        self.date_of_obs_text = wx.StaticText(self, label=self.date_of_obs, name=self.DATE_OF_OBS)

        self.ra_label = wx.StaticText(self, label=self.RA)
        self.ra_text = wx.StaticText(self, label=self.ra_str, name=self.RA)

        self.dec_label = wx.StaticText(self, label=self.DEC)
        self.dec_text = wx.StaticText(self, label=self.dec_str, name=self.DEC)

        self.obs_mag_label = wx.StaticText(self, label=self.OBS_MAG)
        self.obs_mag_text = wx.StaticText(self, label=self.obs_mag, name=self.OBS_MAG)

        self.band_label = wx.StaticText(self, label=self.BAND)
        self.band_text = wx.StaticText(self, label=self.band, name=self.BAND)

        self.observatory_code_label = wx.StaticText(self, label=self.OBSERVATORY_CODE)
        self.observatory_code_text = wx.TextCtrl(self, name=self.OBSERVATORY_CODE)
        self.observatory_code_text.SetValue(self.default_observatory_code)

        self.ok_button = wx.Button(self, label=self.OK_BTN, name=self.OK_BTN)
        self.cancel_button = wx.Button(self, label=self.CANCEL_BTN, name=self.CANCEL_BTN)

        self._do_layout()

    def _get_vertical_widget_list(self):
        return [self._create_horizontal_pair(self.minor_planet_num_label, self.minor_planet_num_text),
                self._create_horizontal_pair(self.provisional_name_label, self.provision_name_text),
                self.discovery_asterisk_cb,
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
                (0, 0)  # blank space
        ]

    def _do_layout(self):
        vsizer = wx.BoxSizer(wx.VERTICAL)
        for widget in self._get_vertical_widget_list():
            vsizer.Add(widget, proportion=0, flag=wx.ALL, border=5)

        vsizer.Add(self._create_horizontal_pair(self.ok_button, self.cancel_button,
                                                flag=wx.ALL, border=5),
                   flag=wx.ALIGN_CENTER)

        # Extra border padding
        bordersizer = wx.BoxSizer(wx.VERTICAL)
        bordersizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(bordersizer)

    def _create_horizontal_pair(self, widget1, widget2, flag=0, border=0):
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(widget1, flag=flag, border=border)
        hsizer.Add(widget2, flag=flag, border=border)
        return hsizer

    def _bind_events(self):
        self.ok_button.Bind(wx.EVT_BUTTON, self._on_ok)
        self.cancel_button.Bind(wx.EVT_BUTTON, self._on_cancel)

    def _on_ok(self, event):
        # Grab data out of the form
        # TODO validation
        minor_planet_number = self.minor_planet_num_text.GetValue()
        discover_asterisk = "*" if self.discovery_asterisk_cb.IsChecked() else " "
        note1 = self.note1_combobox.GetStringSelection()
        note2 = self.note2_combobox.GetStringSelection()
        obs_mag = self.obs_mag
        band = self.band
        observatory_code = self.observatory_code_text.GetValue()

        self.controller.on_do_accept(minor_planet_number,
                                     self.provisional_name,
                                     discover_asterisk,
                                     note1,
                                     note2,
                                     self.date_of_obs,
                                     self.ra_str,
                                     self.dec_str,
                                     obs_mag,
                                     band,
                                     observatory_code
        )

    def _on_cancel(self, event):
        self.controller.on_cancel_accept()

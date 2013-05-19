__author__ = "David Rusk <drusk@uvic.ca>"

import wx


class AcceptSourceDialog(wx.Dialog):

    TITLE = "Accept Source"
    MINOR_PLANET_NUMBER = "Minor planet number: "
    PROVISIONAL_NAME = "Provisional name: "
    DISCOVERY_ASTERISK = "Discovery asterisk: "

    def __init__(self, parent, provisional_name):
        super(AcceptSourceDialog, self).__init__(parent, title=self.TITLE)

        self.provisional_name = provisional_name

        self._init_ui()

    def _init_ui(self):
        self.minor_planet_num_label = wx.StaticText(self, label=self.MINOR_PLANET_NUMBER)
        self.minor_planet_num_text = wx.TextCtrl(self)

        self.provisional_name_label = wx.StaticText(self, label=self.PROVISIONAL_NAME)
        self.provision_name_text = wx.StaticText(self, label=self.provisional_name)

        self.discovery_asterisk_cb = wx.CheckBox(self, label=self.DISCOVERY_ASTERISK,
                                                 style=wx.ALIGN_RIGHT)

        self._do_layout()

    def _get_vertical_widget_list(self):
        return [self._create_horizontal_pair(self.minor_planet_num_label, self.minor_planet_num_text),
                self._create_horizontal_pair(self.provisional_name_label, self.provision_name_text),
                self.discovery_asterisk_cb]

    def _do_layout(self):
        vsizer = wx.BoxSizer(wx.VERTICAL)
        for widget in self._get_vertical_widget_list():
            vsizer.Add(widget, proportion=0, flag=wx.ALL, border=5)

        # Extra border padding
        bordersizer = wx.BoxSizer(wx.VERTICAL)
        bordersizer.Add(vsizer, flag=wx.ALL, border=20)

        self.SetSizerAndFit(bordersizer)

    def _create_horizontal_pair(self, widget1, widget2):
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(widget1)
        hsizer.Add(widget2)
        return hsizer


if __name__ == "__main__":
    # Quick acceptance test

    app = wx.App()
    rootframe = wx.Frame(None)

    # TODO helper test frame/panel for these test dialogs
    # (use in dialogs.py as well)
    panel = wx.Panel(rootframe, wx.ID_ANY)

    def onclick(event):
        AcceptSourceDialog(panel, "provisional-name-1").ShowModal()

    button = wx.Button(panel, id=wx.ID_ANY, label="Press Me")
    button.Bind(wx.EVT_BUTTON, onclick)

    rootframe.Show()
    app.MainLoop()

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.wizard as wiz


class TaskSelectionPage(wiz.PyWizardPage):
    TITLE = "Select a task:"
    TASK_VET = "Vet candidate objects"
    TASK_PROCESS_REAL = "Process real objects"

    def __init__(self, parent):
        super(TaskSelectionPage, self).__init__(parent)

        self._init_ui_components()

    def _init_ui_components(self):
        self.title = wx.StaticText(self, -1, TaskSelectionPage.TITLE)
        self.title.SetFont(wx.Font(18, wx.FONTFAMILY_SWISS, wx.FONTSTYLE_NORMAL,
                                   wx.FONTWEIGHT_BOLD))

        self.vet_candidates_option = wx.RadioButton(
            self, label=TaskSelectionPage.TASK_VET, style=wx.RB_GROUP)
        self.process_reals_option = wx.RadioButton(
            self, label=TaskSelectionPage.TASK_PROCESS_REAL)

        self._do_layout()

    def _do_layout(self):
        border = 5
        selector_sizer = wx.BoxSizer(wx.VERTICAL)
        selector_sizer.Add(self.vet_candidates_option, 0, wx.ALL, border)
        selector_sizer.Add(self.process_reals_option, 0, wx.ALL, border)

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.title, 0, wx.ALIGN_CENTRE | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)
        vsizer.Add(selector_sizer, 0, wx.ALIGN_CENTER | wx.EXPAND | wx.ALL, border)

        self.SetSizer(vsizer)


def run_wizard():
    wizard = wiz.Wizard(None, -1, "Moving Object Pipeline")

    page1 = TaskSelectionPage(wizard)

    wizard.FitToPage(page1)

    wizard.GetPageAreaSizer().Add(page1)

    if wizard.RunWizard(page1):
        wx.MessageBox("Launching main program.", "MOP")
    else:
        wx.MessageBox("Exiting program.", "MOP")

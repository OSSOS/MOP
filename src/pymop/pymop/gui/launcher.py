__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.wizard as wiz


class DynamicLaunchWizardPage(wiz.PyWizardPage):
    def __init__(self, parent, wizard_manager):
        super(DynamicLaunchWizardPage, self).__init__(parent)

        self.wizard_manager = wizard_manager

        self.next = None
        self.prev = None

    def SetNext(self, next):
        self.next = next

    def SetPrev(self, prev):
        self.prev = prev

    def GetNext(self):
        return self.next

    def GetPrev(self):
        return self.prev


class TaskSelectionPage(DynamicLaunchWizardPage):
    TITLE = "Select a task:"
    TASK_VET = "Vet candidate objects"
    TASK_PROCESS_REAL = "Process real objects"

    def __init__(self, parent, wizard_manager):
        super(TaskSelectionPage, self).__init__(parent, wizard_manager)

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

        self._bind_events()

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

    def _bind_events(self):
        self.vet_candidates_option.Bind(wx.EVT_RADIOBUTTON, self._on_select_vet_candidates)
        self.process_reals_option.Bind(wx.EVT_RADIOBUTTON, self._on_select_process_reals)

    def _on_select_vet_candidates(self, event):
        self.wizard_manager.choose_vet_task()

    def _on_select_process_reals(self, event):
        self.wizard_manager.choose_process_reals_task()


class VetCandidatesPage(DynamicLaunchWizardPage):
    TITLE = "Vetting candidate objects"

    def __init__(self, parent, wizard_manager):
        super(VetCandidatesPage, self).__init__(parent, wizard_manager)
        self.wizard_manager = wizard_manager

        self._init_ui_components()

    def _init_ui_components(self):
        self.title = wx.StaticText(self, -1, VetCandidatesPage.TITLE)
        self.title.SetFont(wx.Font(18, wx.FONTFAMILY_SWISS, wx.FONTSTYLE_NORMAL,
                                   wx.FONTWEIGHT_BOLD))

        self._do_layout()

    def _do_layout(self):
        border = 5

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.title, 0, wx.ALIGN_CENTRE | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)

        self.SetSizer(vsizer)


class ProcessRealsPage(DynamicLaunchWizardPage):
    TITLE = "Processing real objects"

    def __init__(self, parent, wizard_manager):
        super(ProcessRealsPage, self).__init__(parent, wizard_manager)
        self.wizard_manager = wizard_manager

        self._init_ui_components()

    def _init_ui_components(self):
        self.title = wx.StaticText(self, -1, ProcessRealsPage.TITLE)

        self.title.SetFont(wx.Font(18, wx.FONTFAMILY_SWISS, wx.FONTSTYLE_NORMAL,
                                   wx.FONTWEIGHT_BOLD))

        self._do_layout()

    def _do_layout(self):
        border = 5

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(self.title, 0, wx.ALIGN_CENTRE | wx.ALL, border)
        vsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND | wx.ALL, border)

        self.SetSizer(vsizer)


class LaunchWizardManager(object):
    def __init__(self):
        self.wizard = wiz.Wizard(None, -1, "Moving Object Pipeline")

        self.task_selection_page = TaskSelectionPage(self.wizard, self)
        self.vet_candidates_page = VetCandidatesPage(self.wizard, self)
        self.process_reals_page = ProcessRealsPage(self.wizard, self)

        # Default selection
        self.task_selection_page.SetNext(self.vet_candidates_page)
        self.vet_candidates_page.prev = self.task_selection_page
        self.process_reals_page.prev = self.task_selection_page

        self.wizard.FitToPage(self.task_selection_page)

        self.wizard.GetPageAreaSizer().Add(self.task_selection_page)

        if self.wizard.RunWizard(self.task_selection_page):
            wx.MessageBox("Launching main program.", "MOP")
        else:
            wx.MessageBox("Exiting program.", "MOP")

    def choose_vet_task(self):
        self.task_selection_page.next = self.vet_candidates_page

    def choose_process_reals_task(self):
        self.task_selection_page.next = self.process_reals_page


def run_wizard():
    LaunchWizardManager()

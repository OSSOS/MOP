import wx
import wx.lib.mixins.listctrl as listmix


class ListCtrlPanel(wx.Panel):
    """
    A list control panel with sortable columns.
    """

    def __init__(self, parent):
        super(ListCtrlPanel, self).__init__(parent)

        self.list = ListCtrlAutoWidth(self,
                                      style=wx.LC_REPORT |
                                            wx.BORDER_NONE |
                                            wx.LC_EDIT_LABELS |
                                            wx.LC_SORT_ASCENDING
        )

        self.list.InsertColumn(0, "Key")
        self.list.InsertColumn(1, "Value")

        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(self.list, 1, wx.EXPAND)
        self.SetSizer(sizer)

    def populate_kv(self, datadict):
        index = 0
        for key, value in datadict.iteritems():
            self.list.InsertStringItem(index, key)
            self.list.SetStringItem(index, 1, value)
            index += 1

        self.list.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        self.list.SetColumnWidth(1, wx.LIST_AUTOSIZE)


class ListCtrlAutoWidth(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin):
    def __init__(self, parent, size=wx.DefaultSize, style=0):
        super(ListCtrlAutoWidth, self).__init__(parent, size=size, style=style)
        listmix.ListCtrlAutoWidthMixin.__init__(self)

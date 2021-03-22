"""
General-purpose list control widgets.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx
from wx.lib.mixins import listctrl as listmix


class ListCtrlAutoWidth(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin):
    def __init__(self, parent, size=wx.DefaultSize, style=0):
        super(ListCtrlAutoWidth, self).__init__(parent, size=size, style=style)
        listmix.ListCtrlAutoWidthMixin.__init__(self)


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

    def populate_list(self, data):
        # Clear any existing data
        self.list.DeleteAllItems()

        item_data_map = {}
        index = 0
        for record in data:
            self.list.InsertItem(index, str(index))
            for colindex, item in enumerate(record):
                self.list.SetItem(index, colindex, str(item))

            # For ColumnSorterMixin
            self.list.SetItemData(index, index)
            item_data_map[index] = record

            index += 1

        self.list.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        self.list.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        # This field is required to be set for the ColumnSorterMixin to work
        self.itemDataMap = item_data_map
        listmix.ColumnSorterMixin.__init__(self, 2)

    def GetListCtrl(self):
        """Required by ColumnSorterMixin"""
        return self.list

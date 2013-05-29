__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.mixins.listctrl as listmix


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

    def populate_list(self, datatuples):
        # Clear any existing data
        self.list.DeleteAllItems()

        item_data_map = {}
        index = 0
        for tuple in datatuples:
            self.list.InsertStringItem(index, str(index))
            for colindex, item in enumerate(tuple):
                self.list.SetStringItem(index, colindex, str(item))

            # For ColumnSorterMixin
            self.list.SetItemData(index, index)
            item_data_map[index] = tuple

            index += 1

        self.list.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        self.list.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        # This field is required to be set for the ColumnSorterMixin to work
        self.itemDataMap = item_data_map
        listmix.ColumnSorterMixin.__init__(self, 2)

    def GetListCtrl(self):
        """Required by ColumnSorterMixin"""
        return self.list


class ListCtrlAutoWidth(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin):
    def __init__(self, parent, size=wx.DefaultSize, style=0):
        super(ListCtrlAutoWidth, self).__init__(parent, size=size, style=style)
        listmix.ListCtrlAutoWidthMixin.__init__(self)


class KeyValueListPanel(ListCtrlPanel):
    def __init__(self, parent, get_data):
        """
        Constructor.

        Args:
          parent: wx.Window
            the parent window of this new panel
          get_data: callable
            when called this should provide the data which will populate
            the key-value list. The returned data should be a list of
            (key, value) tuples.
        """
        super(KeyValueListPanel, self).__init__(parent, ("Key", "Value"))

        self.get_data = get_data

        self.display_data()

    def display_data(self):
        self.populate_list(self.get_data())

    def on_change_data(self, event):
        self.display_data()

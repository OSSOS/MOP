__author__ = "David Rusk <drusk@uvic.ca>"

from mopgui.view.listview import ListCtrlPanel


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

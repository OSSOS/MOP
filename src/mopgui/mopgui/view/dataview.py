__author__ = "David Rusk <drusk@uvic.ca>"

from wx.lib.pubsub import Publisher as pub

from mopgui.model import astrodata
from mopgui.view.list_ctrl import ListCtrlPanel


class AbstractKeyValueListPanel(ListCtrlPanel):
    def __init__(self, parent):
        super(AbstractKeyValueListPanel, self).__init__(parent, ("Key", "Value"))

        pub.subscribe(self.on_change_data, astrodata.MSG_NAV)

        self.display_data()

    def get_data(self):
        """
        Returns a collection of (key, value) tuples to be displayed in a
        list view.
        """
        raise NotImplementedError("This method must be overriden by a subclass")

    def display_data(self):
        self.populate_list(self.get_data())

    def on_change_data(self, event):
        self.display_data()


class ReadingDataView(AbstractKeyValueListPanel):
    def __init__(self, parent, model):
        self.model = model

        super(ReadingDataView, self).__init__(parent)

    def get_data(self):
        return self.model.get_reading_data()


class ObservationHeaderView(AbstractKeyValueListPanel):
    def __init__(self, parent, model):
        self.model = model

        super(ObservationHeaderView, self).__init__(parent)

    def get_data(self):
        return self.model.get_header_data_list()

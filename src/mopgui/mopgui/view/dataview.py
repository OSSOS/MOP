from wx.lib.pubsub import Publisher as pub

from mopgui.model import astrodata
from mopgui.view.list_ctrl import ListCtrlPanel


class ReadingDataView(ListCtrlPanel):
    """
    XXX duplication with ObservationHeaderView
    """

    def __init__(self, parent, model):
        super(ReadingDataView, self).__init__(parent, ("Key", "Value"))

        self.model = model

        pub.subscribe(self.on_change_reading, astrodata.MSG_NAV)

        self.display_data()

    def display_data(self):
        self.populate_list(self.model.get_reading_data())

    def on_change_reading(self, event):
        self.display_data()


class ObservationHeaderView(ListCtrlPanel):
    """
    XXX duplication with ReadingDataView
    """

    def __init__(self, parent, model):
        super(ObservationHeaderView, self).__init__(parent, ("Key", "Value"))

        self.model = model

        pub.subscribe(self.on_change_obs, astrodata.MSG_NAV)

        self.display_data()

    def display_data(self):
        self.populate_list(self.model.get_header_data_list())

    def on_change_obs(self, event):
        self.display_data()

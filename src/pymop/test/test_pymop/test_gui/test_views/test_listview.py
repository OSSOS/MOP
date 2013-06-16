__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

# TODO: upgrade
from wx.lib.pubsub import setupv1
from wx.lib.pubsub import Publisher as pub

from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from pymop.gui import models
from pymop.gui.views import KeyValueListPanel


class ListViewTest(WxWidgetTestCase):
    def setUp(self):
        self.mock_model()

        self.dataset1 = (("Key1", "Val1"), ("Key2", "Val2"), ("Key3", "Val3"))
        self.dataset2 = (("Key4", "Val4"), ("Key5", "Val5"))

        self.model.get_reading_data.return_value = self.dataset1

        # Cause event to be fired when calling next source
        def pub_next_source():
            pub.sendMessage(models.MSG_NEXT_SRC, data=1)

        self.model.next_source = pub_next_source

        self.app = wx.App()
        self.rootframe = wx.Frame(None)

        self.view = KeyValueListPanel(self.rootframe, self.model.get_reading_data)

    def tearDown(self):
        self.rootframe.Destroy()

    def test_kvlist_display_data_startup(self):
        assert_that(self.view.list.GetColumnCount(), equal_to(2))
        assert_that(self.view.list.GetItemCount(), equal_to(3))

        # TODO create custom matcher?
        for item_ind in range(self.view.list.GetItemCount()):
            for col_ind in range(self.view.list.GetColumnCount()):
                assert_that(self.view.list.GetItem(item_ind, col_ind).GetText(),
                            equal_to(self.dataset1[item_ind][col_ind]))

    def test_kvlist_display_data_on_change_reading(self):
        pub.subscribe(self.view.on_change_data, models.MSG_NAV)

        # XXX have to manually update model return value here
        self.model.get_reading_data.return_value = self.dataset2

        self.model.next_source()

        assert_that(self.view.list.GetColumnCount(), equal_to(2))
        assert_that(self.view.list.GetItemCount(), equal_to(2))

        # TODO create custom matcher?
        for item_ind in range(self.view.list.GetItemCount()):
            for col_ind in range(self.view.list.GetColumnCount()):
                assert_that(self.view.list.GetItem(item_ind, col_ind).GetText(),
                            equal_to(self.dataset2[item_ind][col_ind]))


if __name__ == '__main__':
    unittest.main()

import unittest

import wx
from hamcrest import assert_that, equal_to

from test.base_tests import WxWidgetTestCase
from mopgui.view.dataview import ReadingDataView


class DataViewTest(WxWidgetTestCase):
    def setUp(self):
        self.mock_model()

        self.dataset1 = (("Key1", "Val1"), ("Key2", "Val2"), ("Key3", "Val3"))
        self.dataset2 = (("Key4", "Val4"), ("Key4", "Val4"), ("Key4", "Val4"))

        datasets = [self.dataset1, self.dataset2]

        def data_generator():
            index = 0
            retval = datasets[index]
            index += 1
            return retval

        self.model.get_reading_data = data_generator

        self.app = wx.PySimpleApp()
        self.rootframe = wx.Frame(None)

        self.view = ReadingDataView(self.rootframe, self.model)

    def tearDown(self):
        self.rootframe.Destroy()

    def test_reading_data_view_display_data_startup(self):
        assert_that(self.view.list.GetColumnCount(), equal_to(2))
        assert_that(self.view.list.GetItemCount(), equal_to(3))

        # TODO create custom matcher?
        for item_ind in range(self.view.list.GetItemCount()):
            for col_ind in range(self.view.list.GetColumnCount()):
                assert_that(self.view.list.GetItem(item_ind, col_ind).GetText(),
                            equal_to(self.dataset1[item_ind][col_ind]))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from tests.base_tests import WxWidgetTestCase
from ossos.gui.views.listctrls import ListCtrlPanel


class ListViewTest(WxWidgetTestCase):
    def setUp(self):
        super(ListViewTest, self).setUp()

        self.dataset1 = (("Key1", "Val1"), ("Key2", "Val2"), ("Key3", "Val3"))

        self.view = ListCtrlPanel(self.rootframe, ("Col1", "Col2"))

    def test_kvlist_displayed(self):
        self.view.populate_list(self.dataset1)

        assert_that(self.view.list.GetColumnCount(), equal_to(2))
        assert_that(self.view.list.GetItemCount(), equal_to(3))

        # TODO create custom matcher?
        for item_ind in range(self.view.list.GetItemCount()):
            for col_ind in range(self.view.list.GetColumnCount()):
                assert_that(self.view.list.GetItem(item_ind, col_ind).GetText(),
                            equal_to(self.dataset1[item_ind][col_ind]))


if __name__ == '__main__':
    unittest.main()

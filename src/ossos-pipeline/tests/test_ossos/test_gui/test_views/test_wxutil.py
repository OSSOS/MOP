__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from mock import patch
from hamcrest import assert_that, equal_to

from ossos.gui.views.appview import guithread


class WxUtilTest(unittest.TestCase):
    @patch.object(wx, "CallAfter")
    @patch.object(wx, "Thread_IsMain")
    def test_threadsafe_from_worker_thread(
            self, Thread_IsMain_mock, CallAfter_mock):
        # Pretend we are not in the main GUI thread
        Thread_IsMain_mock.return_value = False

        @guithread
        def test_func():
            pass

        test_func()

        assert_that(Thread_IsMain_mock.call_count, equal_to(1))
        assert_that(CallAfter_mock.call_count, equal_to(1))

    @patch.object(wx, "CallAfter")
    @patch.object(wx, "Thread_IsMain")
    def test_threadsafe_from_main_thread(
            self, Thread_IsMain_mock, CallAfter_mock):
        # Pretend we ARE in the main GUI thread
        Thread_IsMain_mock.return_value = True

        @guithread
        def test_func():
            pass

        test_func()

        assert_that(Thread_IsMain_mock.call_count, equal_to(1))
        assert_that(CallAfter_mock.call_count, equal_to(0))


if __name__ == '__main__':
    unittest.main()

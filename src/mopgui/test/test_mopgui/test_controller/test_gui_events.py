__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

import wx

from mock import Mock, patch

from mopgui.controller import gui_events

class ToggleImageLoadingEventTest(unittest.TestCase):

    # @patch.object(TestFrame, "on_event")
    @unittest.skip("Threading issues")
    def test_receive_event(self): #, on_event_mock):
        app = wx.PySimpleApp()
        rootframe = TestFrame(None)
        app.MainLoop()

        # event_handler = Mock()
        # rootframe.event_handler = event_handler

        event = gui_events.ToggleImageLoadingEvent()

        wx.PostEvent(rootframe, event)

        # on_event_mock.assert_called_once_with(event)


class TestFrame(wx.Frame):
    def __init__(self, parent):
        super(TestFrame, self).__init__(parent)

        self.Bind(gui_events.EVT_TOGGLE_IMG_LOADING, self.on_event)

    def on_event(self, event):
        print "Called on_event"


if __name__ == '__main__':
    unittest.main()

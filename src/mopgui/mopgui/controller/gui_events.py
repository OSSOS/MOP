"""
Events for communicating with the GUI mainloop from background worker
threads.

These special events are needed because trying to call GUI code in a thread
other than the mainloop can result in XCB race conditions.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import wx

wxEVT_TOGGLE_IMG_LOADING = wx.NewEventType()
EVT_TOGGLE_IMG_LOADING = wx.PyEventBinder(wxEVT_TOGGLE_IMG_LOADING, 1)

wxEVT_SET_SRC_STATUS = wx.NewEventType()
EVT_SET_SRC_STATUS = wx.PyEventBinder(wxEVT_SET_SRC_STATUS, 1)


class ToggleImageLoadingEvent(wx.PyCommandEvent):
    def __init__(self, should_show, eventType=wxEVT_TOGGLE_IMG_LOADING):
        super(ToggleImageLoadingEvent, self).__init__(eventType)

        self.should_show = should_show


class SetSourceStatusEvent(wx.PyCommandEvent):
    def __init__(self, current_source, total_sources,
                 eventType=wxEVT_SET_SRC_STATUS):
        super(SetSourceStatusEvent, self).__init__(eventType)

        self.current_source = current_source
        self.total_sources = total_sources

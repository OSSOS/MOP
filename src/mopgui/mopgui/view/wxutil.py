__author__ = "David Rusk <drusk@uvic.ca>"

import wx


def threadsafe(function):
    """
    Decorator to make sure the function is called from wx's main GUI
    thread.  This is needed when trying to trigger UI updates from some
    other thread.

    I learned about doing this from:
    wxPython 2.8 Application Development Cookbook, Chapter 11
    """

    def new_threadsafe_function(*args, **kwargs):
        if wx.Thread_IsMain():
            return function(*args, **kwargs)
        else:
            wx.CallAfter(function, *args, **kwargs)

    return new_threadsafe_function

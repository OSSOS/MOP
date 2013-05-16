__author__ = "David Rusk <drusk@uvic.ca>"

import wx


def guithread(function):
    """
    Decorator to make sure the function is called from wx's main GUI
    thread.  This is needed when trying to trigger UI updates from a thread
    other than the main GUI thread (such as some asynchronous data loading
    thread).

    I learned about doing this from:
    wxPython 2.8 Application Development Cookbook, Chapter 11
    """

    def new_guithread_function(*args, **kwargs):
        if wx.Thread_IsMain():
            return function(*args, **kwargs)
        else:
            wx.CallAfter(function, *args, **kwargs)

    return new_guithread_function

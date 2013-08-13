__author__ = "David Rusk <drusk@uvic.ca>"

import wx

from ossos.gui import logger
from ossos.gui.views.dialogs import (should_exit_prompt,
                                     show_empty_workload_dialog)
from ossos.gui.views.errorhandling import CertificateDialog, RetryDownloadDialog
from ossos.gui.views.mainframe import MainFrame
from ossos.gui.views.validation import AcceptSourceDialog, RejectSourceDialog


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


class ApplicationView(object):
    """
    Provides the view's external interface.
    """

    def __init__(self, model, controller):
        self.model = model
        self.controller = controller

        self.mainframe = MainFrame(model, controller)
        # Handle user clicking on the window's "x" button
        self.mainframe.Bind(wx.EVT_CLOSE, self._on_close_window)

        self.accept_source_dialog = None
        self.reject_source_dialog = None
        self.certificate_dialog = None
        self.retry_downloads_dialog = None

        self.mainframe.Show()

        logger.debug("View created.")

    def _on_close_window(self, event):
        self.close()

    @guithread
    def display(self, fits_image, redraw=True):
        self.mainframe.display(fits_image, redraw=redraw)

    @guithread
    def draw_marker(self, x, y, radius, redraw=True):
        self.mainframe.draw_marker(x, y, radius, redraw=redraw)

    @guithread
    def update_displayed_data(self):
        self.mainframe.update_displayed_data()

    @guithread
    def reset_colormap(self):
        self.mainframe.reset_colormap()

    @guithread
    def register_xy_changed_event_handler(self, handler):
        self.mainframe.register_xy_changed_event_handler(handler)

    def close(self):
        self.mainframe.Destroy()

    @guithread
    def show_image_loading_dialog(self):
        self.mainframe.show_image_loading_dialog()

    @guithread
    def hide_image_loading_dialog(self):
        self.mainframe.hide_image_loading_dialog()

    @guithread
    def set_observation_status(self, current_obs, total_obs):
        self.mainframe.set_observation_status(current_obs, total_obs)

    @guithread
    def enable_source_validation(self):
        self.mainframe.enable_validation()

    @guithread
    def disable_source_validation(self):
        self.mainframe.disable_validation()

    def is_source_validation_enabled(self):
        return self.mainframe.is_source_validation_enabled()

    @guithread
    def disable_sync_menu(self):
        self.mainframe.disable_sync_menu()

    @guithread
    def show_certificate_dialog(self, handler, error_message):
        if not self.certificate_dialog:
            self.certificate_dialog = CertificateDialog(self.mainframe,
                                                        handler, error_message)
            self.certificate_dialog.ShowModal()

    @guithread
    def show_retry_download_dialog(self, handler, error_message):
        # Only allow one dialog to be shown at a time
        if not self.retry_downloads_dialog:
            self.retry_downloads_dialog = RetryDownloadDialog(
                self.mainframe, handler, error_message)
            self.retry_downloads_dialog.Show()

    @guithread
    def show_accept_source_dialog(self, provisional_name,
                                  already_discovered,
                                  date_of_obs,
                                  ra,
                                  dec,
                                  obs_mag,
                                  band,
                                  note1_choices=None,
                                  note2_choices=None,
                                  note1_default=None,
                                  note2_default=None,
                                  default_observatory_code="",
                                  default_comment="",
                                  phot_failure=False
    ):
        self.accept_source_dialog = AcceptSourceDialog(
            self.mainframe, self.controller,
            provisional_name,
            already_discovered,
            date_of_obs,
            ra,
            dec,
            obs_mag,
            band,
            note1_choices=note1_choices,
            note2_choices=note2_choices,
            note1_default=note1_default,
            note2_default=note2_default,
            default_observatory_code=default_observatory_code,
            default_comment=default_comment,
            phot_failure=phot_failure)
        self.accept_source_dialog.ShowModal()

    @guithread
    def close_accept_source_dialog(self):
        if self.accept_source_dialog is not None:
            self.accept_source_dialog.Close()
            self.accept_source_dialog = None

    @guithread
    def show_reject_source_dialog(self):
        self.reject_source_dialog = RejectSourceDialog(
            self.mainframe, self.controller)
        self.reject_source_dialog.ShowModal()

    @guithread
    def close_reject_source_dialog(self):
        if self.reject_source_dialog is not None:
            self.reject_source_dialog.Close()
            self.reject_source_dialog = None

    @guithread
    def show_empty_workload_dialog(self):
        show_empty_workload_dialog(self.mainframe, self.model)

    @guithread
    def all_processed_should_exit_prompt(self):
        return should_exit_prompt(self.mainframe)

    @guithread
    def set_autoplay(self, autoplay_enabled):
        self.mainframe.set_autoplay(autoplay_enabled)

    def as_widget(self):
        return self.mainframe

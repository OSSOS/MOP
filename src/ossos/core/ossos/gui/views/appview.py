__author__ = "David Rusk <drusk@uvic.ca>"

import wx
import wx.lib.inspection

from . import dialogs
from .errorhandling import CertificateDialog, RetryDownloadDialog
from .imageview import ImageViewManager
from .keybinds import KeybindManager
from .loading import WaitingGaugeDialog
from .mainframe import MainFrame
from .menu import Menu
from .validation import AcceptSourceDialog, RejectSourceDialog, OffsetSourceDialog, VettingSourceDialog
from .. import logger


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
        # noinspection PyArgumentList
        if wx.IsMainThread():
            return function(*args, **kwargs)
        else:
            wx.CallAfter(function, *args, **kwargs)

    return new_guithread_function


class ApplicationView(object):
    """
    Provides the view's external interface.
    """

    def __init__(self, controller_factory, track_mode=False, debug=False, mark_using_pixels=False, zoom=1):
        logger.debug("Creating Application View")
        self.controller = controller_factory.create_controller(self)

        self.wx_app = wx.App(False)
        logger.debug("App built")
        self.debug = debug
        self.mark_using_pixels = mark_using_pixels

        self.mainframe = MainFrame(self.controller, track_mode=track_mode)
        self.image_view_manager = ImageViewManager(self.mainframe, zoom)
        self.menu = Menu(self.mainframe, self.controller)
        self.keybind_manager = KeybindManager(self.mainframe, self.controller)

        self.loading_dialog = WaitingGaugeDialog(self.mainframe,
                                                 "Image loading...")

        # Handle user clicking on the window's "x" button
        self.mainframe.Bind(wx.EVT_CLOSE, self._on_close_window)

        self.accept_source_dialog = None
        self.reject_source_dialog = None
        self.certificate_dialog = None
        self.retry_downloads_dialog = None
        self.vetting_source_dialog = None
        self.offset_source_dialog = None

        # TODO refactor
        self.register_xy_changed_event_handler(self.controller.on_reposition_source)
        logger.debug("Done.")

    def _on_close_window(self, event):
        if event is not None:
            logger.debug(f'Window close event: {event}')
        self.close()

    @property
    def image_viewer(self):
        return self.image_view_manager.image_viewer

    @guithread
    def show(self):
        self.mainframe.Show()

        if self.debug:
            logger.info("Launching view in debug mode.")
            wx.lib.inspection.InspectionTool().Show()

        self.wx_app.MainLoop()

    @guithread
    def display(self, cutout, use_pixel_coords=False):
        self.image_viewer.display(cutout, use_pixel_coords)

    @guithread
    def place_marker(self, cutout, x, y, radius=10, colour='r', force=False):
        self.image_viewer.place_marker(cutout, x, y, radius, colour, force=force)

    @property
    def ds9(self):
        return self.image_viewer.ds9

    @guithread
    def align(self, cutout, reading, source):
        self.image_viewer.align(cutout, reading, source)

    @guithread
    def clear(self):
        self.image_viewer.clear()

    @guithread
    def frame(self, frame):
        return self.image_viewer.ds9.set('frame {}'.format(frame))

    @guithread
    def refresh_markers(self):
        self.image_viewer.refresh_markers()

    @guithread
    def draw_uncertainty_ellipse(self, cutout):
        self.image_viewer.draw_uncertainty_ellipse(cutout)

    @guithread
    def mark_apertures(self, cutout, pixel=False):
        self.image_viewer.mark_apertures(cutout, pixel)

    @guithread
    def reset_colormap(self):
        self.image_viewer.reset_colormap()

    @guithread
    def toggle_reticule(self):
        self.image_viewer.toggle_reticule()

    @guithread
    def register_xy_changed_event_handler(self, handler):
        self.image_viewer.register_xy_changed_event_handler(handler)

    @guithread
    def show_image_loading_dialog(self):
        if not self.loading_dialog.IsShown():
            self.loading_dialog.CenterOnParent()
            self.loading_dialog.Show()

    @guithread
    def hide_image_loading_dialog(self):
        if self.loading_dialog.IsShown():
            self.loading_dialog.Hide()

    @guithread
    def use_singlets(self):
        self.image_view_manager.use_singlets()

    @guithread
    def use_triplets(self):
        self.image_view_manager.use_triplets()

    @guithread
    def update_displayed_data(self, reading_data, header_data_list):
        self.mainframe.update_displayed_data(reading_data, header_data_list)

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
        self.menu.disable_sync()

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
                                  date_of_obs,
                                  ra,
                                  dec,
                                  obs_mag,
                                  obs_mag_err,
                                  band,
                                  note1_choices=None,
                                  note2_choices=None,
                                  note1_default=None,
                                  note2_default=None,
                                  default_observatory_code="",
                                  default_comment="",
                                  phot_failure=False,
                                  pixel_x=None,
                                  pixel_y=None
                                  ):
        self.accept_source_dialog = AcceptSourceDialog(
            self.mainframe, self.controller,
            provisional_name,
            date_of_obs,
            ra,
            dec,
            obs_mag,
            obs_mag_err,
            band,
            note1_choices=note1_choices,
            note2_choices=note2_choices,
            note1_default=note1_default,
            note2_default=note2_default,
            default_observatory_code=default_observatory_code,
            default_comment=default_comment,
            phot_failure=phot_failure,
            pixel_x=pixel_x,
            pixel_y=pixel_y)
        self.accept_source_dialog.ShowModal()

    @guithread
    def close_accept_source_dialog(self):
        if self.accept_source_dialog is not None:
            self.accept_source_dialog.Close()
            self.accept_source_dialog = None

    @guithread
    def show_vetting_accept_source_dialog(self):
        self.vetting_source_dialog = VettingSourceDialog(
            self.mainframe, self.controller)
        self.vetting_source_dialog.ShowModal()

    @guithread
    def close_vetting_accept_source_dialog(self):
        if self.vetting_source_dialog is not None:
            self.vetting_source_dialog.Close()
            self.vetting_source_dialog = None

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
    def show_offset_source_dialog(self, cen_coords, pix_coords):
        self.offset_source_dialog = OffsetSourceDialog(
            self.mainframe, self.controller, cen_coords, pix_coords)
        self.offset_source_dialog.ShowModal()

    @guithread
    def close_offset_source_dialog(self):
        if self.offset_source_dialog is not None:
            self.offset_source_dialog.Close()
            self.offset_source_dialog = None

    @guithread
    def show_keymappings(self):
        dialogs.show_keymappings_dialog(self.mainframe, self.keybind_manager)

    @guithread
    def show_empty_workload_dialog(self, directory):
        dialogs.show_empty_workload_dialog(self.mainframe, directory)

    @guithread
    def all_processed_should_exit_prompt(self):
        return dialogs.should_exit_prompt(self.mainframe)

    @guithread
    def set_autoplay(self, autoplay_enabled):
        self.menu.set_autoplay(autoplay_enabled)

    def as_widget(self):
        return self.mainframe

    def close(self):
        self.mainframe.Destroy()

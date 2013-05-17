"""
Main controller of the application.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from wx.lib.pubsub import Publisher as pub

from mopgui.model import astrodata
from mopgui.view.appview import ApplicationView
from mopgui.controller.validationcontrol import SourceValidationController


class ApplicationController(object):
    """
    The top-level controller of the application.  Sets up the views and other
    controllers, and handles high level events like exiting.
    """

    def __init__(self, model, output_writer, name_generator, image_viewer,
                 debug_mode=False, unittest=False):
        self.model = model
        self.output_writer = output_writer
        self.image_viewer = image_viewer

        # set up the more fine-grained controllers
        self.validationcontroller = SourceValidationController(self.model, self.output_writer, name_generator)
        self.navcontroller = NavigationController(self.model)

        pub.subscribe(self.on_change_image, astrodata.MSG_NAV)
        pub.subscribe(self.on_image_loaded, astrodata.MSG_IMG_LOADED)

        self.view = ApplicationView(self.model, self, self.validationcontroller,
                                    self.navcontroller)
        self.view.launch(debug_mode=debug_mode, unittest=unittest)

    def get_view(self):
        return self.view

    def display_current_image(self):
        current_image = self.model.get_current_image()

        if current_image is None:
            self.get_view().show_image_loading_dialog()
        else:
            self.image_viewer.view_image(current_image)
            image_x, image_y = self.model.get_current_image_source_point()
            radius = 2 * round(self.model.get_current_image_FWHM())
            self.image_viewer.draw_circle(image_x, image_y, radius)

        # Add 1 so displayed source numbers don't start at 0
        self.get_view().set_source_status(
            self.model.get_current_source_number() + 1,
            self.model.get_source_count())

    def on_image_loaded(self, event):
        source_num, obs_num = event.data
        if (self.model.get_current_source_number() == source_num and
                    self.model.get_current_obs_number() == obs_num):
            self.get_view().hide_image_loading_dialog()
            self.display_current_image()

    def on_change_image(self, event):
        self.display_current_image()

    def on_exit(self, event):
        self.image_viewer.close()
        self.view.close()


class NavigationController(object):
    def __init__(self, model):
        self.model = model

    def on_next_source(self, event):
        self.model.next_source()

    def on_previous_source(self, event):
        self.model.previous_source()
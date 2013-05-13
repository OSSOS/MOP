"""
Main controller of the application.
"""

from mopgui.view.appview import ApplicationView


class ApplicationController(object):
    """
    The top-level controller of the application.  Sets up the views and other
    controllers, and handles high level events like exiting.
    """

    def __init__(self, model, image_viewer, debug_mode=False, unittest=False):
        self.model = model
        self.image_viewer = image_viewer

        # set up the more fine-grained controllers
        self.navcontroller = NavigationController(self.model)

        self.view = ApplicationView(self.model, self, self.navcontroller,
                                    self.image_viewer)
        self.view.launch(debug_mode=debug_mode, unittest=unittest)

    def get_view(self):
        return self.view

    def on_change_image(self, event):
        self.view.display_current_image()

    def on_exit(self, event):
        self.view.close()


class NavigationController(object):
    def __init__(self, model):
        self.model = model

    def on_next_source(self, event):
        self.model.next_source()

    def on_previous_source(self, event):
        self.model.previous_source()
__author__ = "David Rusk <drusk@uvic.ca>"

from wx.lib.pubsub import Publisher as pub

from pymop import config
from pymop.gui import models
from pymop.gui.views import ApplicationView


class ApplicationController(object):
    """
    The main controller of the application.  Sets up the view and
    handles user interactions.
    """

    def __init__(self, model, output_writer, name_generator,
                 debug_mode=False, unittest=False):
        self.unittest = unittest

        self.model = model
        self.output_writer = output_writer
        self.name_generator = name_generator

        pub.subscribe(self.on_change_image, models.MSG_NAV)
        pub.subscribe(self.on_image_loaded, models.MSG_IMG_LOADED)
        pub.subscribe(self.on_all_sources_processed, models.MSG_ALL_SRC_PROC)

        self.view = ApplicationView(self.model, self)
        self.view.launch(debug_mode=debug_mode, unittest=unittest)

    def get_view(self):
        return self.view

    def get_model(self):
        return self.model

    def display_current_image(self):
        current_image = self.model.get_current_image()

        if current_image is None:
            self.get_view().show_image_loading_dialog()
        else:
            self.get_view().view_image(current_image)
            image_x, image_y = self.model.get_current_image_source_point()
            radius = 2 * round(self.model.get_current_image_FWHM())
            self.get_view().draw_circle(image_x, image_y, radius)

        # Add 1 so displayed source numbers don't start at 0
        self.get_view().set_source_status(
            self.model.get_current_source_number() + 1,
            self.model.get_source_count())
        self.get_view().set_observation_status(
            self.model.get_current_obs_number() + 1,
            self.model.get_obs_count())

    def on_image_loaded(self, event):
        source_num, obs_num = event.data
        if (self.model.get_current_source_number() == source_num and
                    self.model.get_current_obs_number() == obs_num):
            self.get_view().hide_image_loading_dialog()
            self.display_current_image()
        self.get_view().set_loading_status(self.model.get_loaded_image_count(),
                                           self.model.get_total_image_count())

    def on_change_image(self, event):
        self.display_current_image()

    def on_all_sources_processed(self, event):
        # TODO: yuck, refactor
        if not self.unittest:
            should_exit = self.get_view().all_processed_should_exit_prompt()
            if should_exit:
                self._do_exit()

    def on_exit(self):
        self._do_exit()

    def _do_exit(self):
        self.view.close()

    def on_next_obs(self):
        self.model.next_obs()

    def on_previous_obs(self):
        self.model.previous_obs()

    def _get_provisional_name(self):
        return self.name_generator.generate_name(
            self.model.get_current_exposure_number())

    def on_initiate_accept(self):
        """Initiates acceptance procedure, gathering required data."""
        preset_vals = (
            self._get_provisional_name(),
            self.model.get_current_observation_date(),
            self.model.get_current_ra(),
            self.model.get_current_dec(),
            self.model.get_current_source_observed_magnitude(),
            self.model.get_current_band(),
            config.read("MPC.NOTE1OPTIONS"),
            config.read("MPC.NOTE2OPTIONS"),
            config.read("MPC.NOTE2DEFAULT"),
            config.read("MPC.DEFAULT_OBSERVATORY_CODE")
        )
        self.get_view().show_accept_source_dialog(preset_vals)

    def on_reject(self):
        self.model.set_current_source_processed()
        self.model.next_source()

    def on_do_accept(self,
                     minor_plant_number,
                     provisional_name,
                     discovery_asterisk,
                     note1,
                     note2,
                     date_of_ob,
                     ra,
                     dec,
                     obs_mag,
                     band,
                     observatory_code):
        """Final acceptance with collected data."""
        # Just extract the character code from the notes, not the
        # full description
        note1_code = note1.split(" ")[0]
        note2_code = note2.split(" ")[0]
        self.output_writer.write_line(
            minor_plant_number,
            provisional_name,
            discovery_asterisk,
            note1_code,
            note2_code,
            date_of_ob,
            ra,
            dec,
            obs_mag,
            band,
            observatory_code)

        self.get_view().close_accept_source_dialog()
        self.model.set_current_source_processed()
        self.model.next_source()

    def on_cancel_accept(self):
        self.get_view().close_accept_source_dialog()

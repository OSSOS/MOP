__author__ = "David Rusk <drusk@uvic.ca>"

from pymop import config
from pymop.gui import events
from pymop.gui.views import ApplicationView


class AbstractController(object):
    def __init__(self, task, model):
        self.task = task
        self.model = model

        events.subscribe(events.NAV, self.on_change_image)
        events.subscribe(events.IMG_LOADED, self.on_image_loaded)
        events.subscribe(events.NO_AVAILABLE_WORK, self.on_no_available_work)

        self.view = ApplicationView(self.model, self)

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

        self.get_view().set_observation_status(
            self.model.get_current_obs_number() + 1,
            self.model.get_obs_count())

    def on_image_loaded(self, event):
        source_num, obs_num = event.data
        if (self.model.get_current_source_number() == source_num and
                    self.model.get_current_obs_number() == obs_num):
            self.get_view().hide_image_loading_dialog()
            self.display_current_image()

    def on_change_image(self, event):
        if self.model.is_current_item_processed():
            self.get_view().disable_source_validation()
        else:
            self.get_view().enable_source_validation()

        self.display_current_image()

    def on_no_available_work(self, event):
        should_exit = self.get_view().all_processed_should_exit_prompt()
        if should_exit:
            self._do_exit()

    def on_exit(self):
        self._do_exit()

    def _do_exit(self):
        self.view.close()
        self.model.exit()
        self.task.finish()

    def on_next_obs(self):
        self.model.next_obs()

    def on_previous_obs(self):
        self.model.previous_obs()

    def on_accept(self):
        raise NotImplementedError()

    def on_reject(self):
        raise NotImplementedError()


class ProcessRealsController(AbstractController):
    """
    The main controller of the process reals task.  Sets up the view and
    handles user interactions.
    """

    def __init__(self, task, model, name_generator):
        super(ProcessRealsController, self).__init__(task, model)

        self.name_generator = name_generator

    def _get_provisional_name(self):
        return self.name_generator.name_source(self.model.get_current_source())

    def on_accept(self):
        """Initiates acceptance procedure, gathering required data."""
        preset_vals = (
            self._get_provisional_name(),
            self.model.is_current_source_discovered(),
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
                     observatory_code,
                     comment):
        """Final acceptance with collected data."""
        # Just extract the character code from the notes, not the
        # full description
        note1_code = note1.split(" ")[0]
        note2_code = note2.split(" ")[0]

        writer = self.model.get_writer()
        writer.write_comment(self.model.get_current_reading(), comment)
        writer.write_mpc_line(
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
        self.model.accept_current_item()
        self.model.next_item()

    def on_cancel_accept(self):
        self.get_view().close_accept_source_dialog()

    def on_reject(self):
        self.get_view().show_reject_source_dialog()

    def on_do_reject(self, comment):
        writer = self.model.get_writer()
        writer.write_comment(self.model.get_current_reading(), comment)
        writer.write_rejection_line(self.model.get_current_observation_date(),
                                    self.model.get_current_ra(),
                                    self.model.get_current_dec())

        self.get_view().close_reject_source_dialog()
        self.model.reject_current_item()
        self.model.next_item()

    def on_cancel_reject(self):
        self.get_view().close_reject_source_dialog()


class ProcessCandidatesController(AbstractController):
    def __init__(self, task, model):
        super(ProcessCandidatesController, self).__init__(task, model)

    def on_accept(self):
        self.model.get_writer().write_source(self.model.get_current_source())

        self.model.accept_current_item()
        self.model.next_item()

    def on_reject(self):
        self.model.reject_current_item()
        self.model.next_item()

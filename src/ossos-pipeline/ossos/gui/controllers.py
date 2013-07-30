__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.daophot import TaskError
from ossos.gui import events, config
from ossos.gui.views import ApplicationView
from ossos.gui.models import ImageNotLoadedException


class AbstractController(object):
    def __init__(self, model):
        self.model = model

        events.subscribe(events.CHANGE_IMAGE, self.on_change_image)
        events.subscribe(events.IMG_LOADED, self.on_image_loaded)
        events.subscribe(events.NO_AVAILABLE_WORK, self.on_no_available_work)

        self.view = ApplicationView(self.model, self)
        self.view.register_xy_changed_event_handler(self.on_reposition_source)

    def get_view(self):
        return self.view

    def get_model(self):
        return self.model

    def display_current_image(self):
        view = self.get_view()

        try:
            view.view_image(self.get_model().get_current_image(),
                            redraw=False)
        except ImageNotLoadedException:
            view.show_image_loading_dialog()
            return

        self.circle_current_source()

        view.update_displayed_data()

        view.set_observation_status(
            self.model.get_current_obs_number() + 1,
            self.model.get_obs_count())

    def circle_current_source(self):
        image_x, image_y = self.model.get_current_pixel_source_point()
        radius = 2 * round(self.model.get_current_image_FWHM())
        self.get_view().draw_circle(image_x, image_y, radius, redraw=True)

    def on_reposition_source(self, new_x, new_y):
        self.model.update_current_source_location((new_x, new_y))

    def on_image_loaded(self, event):
        source_reading = event.data
        if source_reading == self.model.get_current_reading():
            self.get_view().hide_image_loading_dialog()
            self.display_current_image()

    def on_change_image(self, event):
        if self.model.is_current_item_processed():
            self.get_view().disable_source_validation()
        else:
            self.get_view().enable_source_validation()

        self.display_current_image()

    def on_no_available_work(self, event):
        self.get_view().hide_image_loading_dialog()

        if self.model.get_num_items_processed() == 0:
            self.get_view().show_empty_workload_dialog()
            self._do_exit()
        else:
            should_exit = self.get_view().all_processed_should_exit_prompt()
            if should_exit:
                self._do_exit()

    def on_enable_auto_sync(self):
        self.model.enable_synchronization()

    def on_disable_auto_sync(self):
        self.model.disable_synchronization()

    def on_exit(self):
        self._do_exit()

    def _do_exit(self):
        self.view.close()
        self.model.exit()

    def on_next_obs(self):
        self.model.next_obs()

    def on_previous_obs(self):
        self.model.previous_obs()

    def on_accept(self):
        raise NotImplementedError()

    def on_reject(self):
        raise NotImplementedError()

    def on_reset_source_location(self):
        self.model.reset_current_source_location()
        self.circle_current_source()


class ProcessRealsController(AbstractController):
    """
    The main controller of the process reals task.  Sets up the view and
    handles user interactions.
    """

    def __init__(self, model, name_generator):
        super(ProcessRealsController, self).__init__(model)

        self.name_generator = name_generator

    def _generate_provisional_name(self):
        return self.name_generator.generate_name(
            self.model.get_current_astrom_header(),
            self.model.get_current_fits_header())

    def on_accept(self):
        """
        Initiates acceptance procedure, gathering required data.
        """
        if self.model.is_current_source_named():
            provisional_name = self.model.get_current_source_name()
        else:
            provisional_name = self._generate_provisional_name()

        band = self.model.get_current_band()
        default_comment = ""
        phot_failure = False

        try:
            obs_mag = self.model.get_current_source_observed_magnitude()
        except TaskError as error:
            phot_failure = True
            obs_mag = ""
            band = ""
            default_comment = str(error)

        if self.model.is_current_source_adjusted():
            note1_default = config.read("MPC.NOTE1_HAND_ADJUSTED")
        else:
            note1_default = None

        self.get_view().show_accept_source_dialog(
            provisional_name,
            self.model.is_current_source_discovered(),
            self.model.get_current_observation_date(),
            self.model.get_current_ra(),
            self.model.get_current_dec(),
            obs_mag,
            band,
            note1_choices=config.read("MPC.NOTE1OPTIONS"),
            note2_choices=config.read("MPC.NOTE2OPTIONS"),
            note1_default=note1_default,
            note2_default=config.read("MPC.NOTE2DEFAULT"),
            default_observatory_code=config.read("MPC.DEFAULT_OBSERVATORY_CODE"),
            default_comment=default_comment,
            phot_failure=phot_failure
        )

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
                     comment,
                     phot_failure):
        """
        Final acceptance with collected data.
        """
        # Just extract the character code from the notes, not the
        # full description
        note1_code = note1.split(" ")[0]
        note2_code = note2.split(" ")[0]

        self.get_view().close_accept_source_dialog()

        self.model.set_current_source_name(provisional_name)

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
            observatory_code,
            phot_failure=phot_failure)

        self.model.accept_current_item()
        self.model.next_item()

    def on_cancel_accept(self):
        self.get_view().close_accept_source_dialog()

    def on_reject(self):
        self.get_view().show_reject_source_dialog()

    def on_do_reject(self, comment):
        self.get_view().close_reject_source_dialog()

        if not self.model.is_current_source_named():
            self.model.set_current_source_name(self._generate_provisional_name())

        writer = self.model.get_writer()
        writer.write_comment(self.model.get_current_reading(), comment)
        writer.write_rejection_line(self.model.get_current_observation_date(),
                                    self.model.get_current_ra(),
                                    self.model.get_current_dec())

        self.model.reject_current_item()
        self.model.next_item()

    def on_cancel_reject(self):
        self.get_view().close_reject_source_dialog()


class ProcessCandidatesController(AbstractController):
    def __init__(self, model):
        super(ProcessCandidatesController, self).__init__(model)

    def on_accept(self):
        writer = self.model.get_writer()
        writer.write_source(self.model.get_current_source())

        self.model.accept_current_item()
        self.model.next_item()

    def on_reject(self):
        self.model.reject_current_item()
        self.model.next_item()

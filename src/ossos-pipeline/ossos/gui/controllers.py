__author__ = "David Rusk <drusk@uvic.ca>"

from ossos import mpc
from ossos.daophot import TaskError
from ossos.gui import config
from ossos.gui import events
from ossos.gui.autoplay import AutoplayManager
from ossos.gui.models import ImageNotLoadedException, NoWorkUnitException
from ossos.gui.views.app import ApplicationView


class AbstractController(object):
    def __init__(self, model):
        self.model = model

        events.subscribe(events.CHANGE_IMAGE, self.on_change_image)
        events.subscribe(events.IMG_LOADED, self.on_image_loaded)
        events.subscribe(events.NO_AVAILABLE_WORK, self.on_no_available_work)

        self.view = ApplicationView(self.model, self)
        self.view.register_xy_changed_event_handler(self.on_reposition_source)

        self.autoplay_manager = AutoplayManager(model)
        self.image_loading_dialog_manager = ImageLoadingDialogManager(self.view)

    def get_view(self):
        return self.view

    def display_current_image(self):
        try:
            self.view.display(self.model.get_current_displayable_item(),
                              redraw=False)
        except ImageNotLoadedException:
            self.image_loading_dialog_manager.wait_for_item(
                self.model.get_current_reading())
            return
        except NoWorkUnitException:
            return

        self.mark_current_source()

        self.view.update_displayed_data()

        self.view.set_observation_status(
            self.model.get_current_obs_number() + 1,
            self.model.get_obs_count())

        self.model.acknowledge_image_displayed()

    def mark_current_source(self):
        image_x, image_y = self.model.get_current_pixel_source_point()
        radius = 2 * round(self.model.get_current_image_FWHM())
        self.view.draw_marker(image_x, image_y, radius, redraw=True)

    def on_reposition_source(self, new_x, new_y):
        try:
            self.model.update_current_source_location((new_x, new_y))
        except ImageNotLoadedException:
            pass

    def on_image_loaded(self, event):
        source_reading = event.data
        self.image_loading_dialog_manager.set_item_done(source_reading)

        if source_reading == self.model.get_current_reading():
            self.display_current_image()

    def on_change_image(self, event):
        if self.model.is_current_item_processed():
            self.view.disable_source_validation()
        else:
            self.view.enable_source_validation()

        self.display_current_image()

    def on_no_available_work(self, event):
        self.view.hide_image_loading_dialog()

        if self.model.get_num_items_processed() == 0:
            self.view.show_empty_workload_dialog()
            self._do_exit()
        else:
            should_exit = self.view.all_processed_should_exit_prompt()
            if should_exit:
                self._do_exit()

    def on_enable_auto_sync(self):
        self.model.enable_synchronization()

    def on_disable_auto_sync(self):
        self.model.disable_synchronization()

    def on_enable_autoplay(self):
        self.autoplay_manager.start_autoplay()

    def on_disable_autoplay(self):
        self.autoplay_manager.stop_autoplay()

    def on_toggle_autoplay_key(self):
        """
        The user has pressed the keybind for toggling autoplay.
        """
        if self.autoplay_manager.is_running():
            self.autoplay_manager.stop_autoplay()
            self.view.set_autoplay(False)
        else:
            self.autoplay_manager.start_autoplay()
            self.view.set_autoplay(True)

    def on_exit(self):
        self._do_exit()

    def _do_exit(self):
        self.autoplay_manager.stop_autoplay()
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
        try:
            self.model.reset_current_source_location()
            self.mark_current_source()
        except ImageNotLoadedException:
            pass


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

        self.view.show_accept_source_dialog(
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
                     minor_planet_number,
                     provisional_name,
                     discovery_asterisk,
                     note1,
                     note2,
                     date_of_obs,
                     ra,
                     dec,
                     obs_mag,
                     band,
                     observatory_code,
                     comment):
        """
        Final acceptance with collected data.
        """
        # Just extract the character code from the notes, not the
        # full description
        note1_code = note1.split(" ")[0]
        note2_code = note2.split(" ")[0]

        self.view.close_accept_source_dialog()

        self.model.set_current_source_name(provisional_name)

        mpc_observation = mpc.Observation(
            minor_planet_number=minor_planet_number,
            provisional_name=provisional_name,
            discovery=discovery_asterisk,
            note1=note1_code,
            note2=note2_code,
            date=date_of_obs,
            ra=ra,
            dec=dec,
            mag=obs_mag,
            band=band,
            observatory_code=observatory_code,
            comment=self.generate_mpc_comment(comment))

        self.model.get_writer().write(mpc_observation)

        self.model.accept_current_item()
        self.model.next_item()

    def on_cancel_accept(self):
        self.view.close_accept_source_dialog()

    def on_reject(self):
        self.view.show_reject_source_dialog()

    def on_do_reject(self, comment):
        self.view.close_reject_source_dialog()

        if not self.model.is_current_source_named():
            self.model.set_current_source_name(self._generate_provisional_name())

        mpc_observation = mpc.Observation(
            provisional_name=self.model.get_current_source_name(),
            date=self.model.get_current_observation_date(),
            ra=self.model.get_current_ra(),
            dec=self.model.get_current_dec(),
            comment=self.generate_mpc_comment(comment))

        mpc_observation.null_observation = True

        self.model.get_writer().write(mpc_observation)

        self.model.reject_current_item()
        self.model.next_item()

    def on_cancel_reject(self):
        self.view.close_reject_source_dialog()

    def generate_mpc_comment(self, comment):
        reading = self.model.get_current_reading()
        return "%s %s %s %s\n" % (
            reading.obs.rawname, reading.x, reading.y, comment)


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


class ImageLoadingDialogManager(object):
    def __init__(self, view):
        self.view = view
        self._wait_items = set()
        self._dialog_showing = False

    def wait_for_item(self, item):
        self._wait_items.add(item)

        if not self._dialog_showing:
            self.view.show_image_loading_dialog()
            self._dialog_showing = True

    def set_item_done(self, item):
        if item not in self._wait_items:
            return

        self._wait_items.remove(item)

        if len(self._wait_items) == 0 and self._dialog_showing:
            self.view.hide_image_loading_dialog()
            self._dialog_showing = False

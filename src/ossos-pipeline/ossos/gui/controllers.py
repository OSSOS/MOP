import math
from ossos.fitsviewer.displayable import Marker

__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.daophot import TaskError
from ossos.gui.autoplay import AutoplayManager
from ossos.gui import config, logger
from ossos.gui import events
from ossos.gui.models.exceptions import (ImageNotLoadedException,
                                         NoWorkUnitException)
from ossos import mpc
from ossos.orbfit import OrbfitError


class AbstractController(object):
    def __init__(self, model, view):
        self.model = model
        self.view = view

        events.subscribe(events.CHANGE_IMAGE, self.on_change_image)
        events.subscribe(events.IMG_LOADED, self.on_image_loaded)
        events.subscribe(events.NO_AVAILABLE_WORK, self.on_no_available_work)

        self.autoplay_manager = AutoplayManager(model)
        self.image_loading_dialog_manager = ImageLoadingDialogManager(view)

    def get_view(self):
        return self.view

    def display_current_image(self):
        try:
            self.view.display(self.model.get_current_cutout())
        except ImageNotLoadedException as ex:
            self.image_loading_dialog_manager.wait_for_item(ex.requested_item)
            return False
        except NoWorkUnitException:
            return False

        self.view.update_displayed_data(self.model.get_reading_data(),
                                        self.model.get_header_data_list())

        self.view.set_observation_status(
            self.model.get_current_obs_number() + 1,
            self.model.get_obs_count())

        self.model.acknowledge_image_displayed()

        return True

    def on_reposition_source(self, new_x, new_y):
        try:
            self.model.update_current_source_location((new_x, new_y))
        except ImageNotLoadedException:
            pass

    def on_image_loaded(self, event):
        displayable_item = event.data
        self.image_loading_dialog_manager.set_item_done(displayable_item)

        if displayable_item == self.model.get_current_displayable_item():
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
            self.view.show_empty_workload_dialog(
                self.model.get_working_directory())
            self._do_exit()
        else:
            should_exit = self.view.all_processed_should_exit_prompt()
            if should_exit:
                self._do_exit()

    def on_use_singlet_view(self):
        self.model.use_singlets()
        self.view.use_singlets()
        self.display_current_image()

    def on_use_triplet_view(self):
        self.model.use_triplets()
        self.view.use_triplets()
        self.display_current_image()

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

    def on_toggle_reticule_key(self):
        self.view.toggle_reticule()

    def on_show_keymappings(self):
        self.view.show_keymappings()

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
            self.view.refresh_markers()
        except ImageNotLoadedException:
            pass

    def on_reset_colormap(self):
        self.view.reset_colormap()


class ProcessRealsController(AbstractController):
    """
    The main controller of the process reals task.  Sets up the view and
    handles user interactions.
    """

    def __init__(self, model, view, name_generator):
        super(ProcessRealsController, self).__init__(model, view)

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

        source_cutout = self.model.get_current_cutout()
        pixel_x = source_cutout.pixel_x
        pixel_y = source_cutout.pixel_y

        try:
            cen_x, cen_y, obs_mag, obs_mag_err = self.model.get_current_source_observed_magnitude()
        except TaskError as error:
            phot_failure = True
            obs_mag = ""
            cen_x = pixel_x
            cen_y = pixel_y
            obs_mag_err = -1
            band = ""
            default_comment = str(error)

        if math.sqrt( (cen_x - pixel_x)**2 + (cen_y - pixel_y)**2 ) > 1.5:
            # check if the user wants to use the 'hand' coordinates or these new ones.
            self.view.draw_error_ellipse(cen_x, cen_y, 10, 10, 0, color='r')
            self.view.show_offset_source_dialog((cen_x, cen_y), (pixel_x,pixel_y))
        else:
            source_cutout.update_pixel_location((cen_x, cen_y))
            self.model.get_current_cutout()._adjusted = False

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
            obs_mag_err,
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
                     obs_mag_err,
                     band,
                     observatory_code,
                     comment,
    ):
        """
        Final acceptance with collected data.
        """
        # Just extract the character code from the notes, not the
        # full description
        note1_code = note1.split(" ")[0]
        note2_code = note2.split(" ")[0]

        self.view.close_accept_source_dialog()

        self.model.set_current_source_name(provisional_name)

        reading = self.model.get_current_reading()

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
            mag_err=obs_mag_err,
            band=band,
            observatory_code=observatory_code,
            comment=comment,
            xpos=reading.x,
            ypos=reading.y,
            frame=reading.obs.rawname)

        self.model.get_writer().write(mpc_observation)

        self.model.accept_current_item()
        self.model.next_item()

    def on_cancel_accept(self):
        self.view.close_accept_source_dialog()

    def on_reject(self):
        self.view.show_reject_source_dialog()

    def on_do_offset(self, cen_coords):
        self.view.close_offset_source_dialog()

        source_cutout = self.model.get_current_cutout()
        source_cutout.update_pixel_location(cen_coords)
        source_cutout._adjusted = False

    def on_cancel_offset(self, pix_coords):
        self.view.close_offset_source_dialog()

        source_cutout = self.model.get_current_cutout()
        source_cutout.update_pixel_location(pix_coords)


    def on_do_reject(self, comment):
        self.view.close_reject_source_dialog()

        if not self.model.is_current_source_named():
            self.model.set_current_source_name(self._generate_provisional_name())

        reading = self.model.get_current_reading()

        mpc_observation = mpc.Observation(
            provisional_name=self.model.get_current_source_name(),
            date=self.model.get_current_observation_date(),
            ra=self.model.get_current_ra(),
            dec=self.model.get_current_dec(),
            xpos=reading.x,
            ypos=reading.y,
            frame=reading.obs.rawname,
            comment=comment)

        mpc_observation.null_observation = True

        self.model.get_writer().write(mpc_observation)

        self.model.reject_current_item()
        self.model.next_item()

    def on_cancel_reject(self):
        self.view.close_reject_source_dialog()


class ProcessCandidatesController(AbstractController):
    def on_accept(self):
        writer = self.model.get_writer()
        writer.write_source(self.model.get_current_source())

        self.model.accept_current_item()
        self.model.next_item()

    def on_reject(self):
        self.model.reject_current_item()
        self.model.next_item()


class ProcessTracksController(ProcessRealsController):
    """
    The main controller of the 'track' task.  Sets up the view and
    handles user interactions. This task extends orbit linkages from
    three out to more observations.
    """

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
                     obs_mag_err,
                     band,
                     observatory_code,
                     comment,
    ):
        super(ProcessTracksController, self).on_do_accept(
            minor_planet_number,
            provisional_name,
            discovery_asterisk,
            note1,
            note2,
            date_of_obs,
            ra,
            dec,
            obs_mag,
            obs_mag_err,
            band,
            observatory_code,
            comment)

        try:
            self.model.get_current_workunit().print_orbfit_info()
        except OrbfitError as error:
            print str(error)

    def display_current_image(self):
        successful = super(ProcessTracksController, self).display_current_image()

        if successful:
            ## Also draw an error ellipse, since this is a tracks controller.
            reading = self.model.get_current_reading()

            if not hasattr(reading, 'redraw_ellipse'):
                reading.redraw_ellipse = True

            if hasattr(reading, 'dra') and hasattr(reading, 'ddec') and hasattr(reading,
                                                                                'pa') and reading.redraw_ellipse:
                x, y = self.model.get_current_pixel_source_point()
                self.view.draw_error_ellipse(x, y, reading.dra, reading.ddec, reading.pa)

            reading.redraw_ellipse = False

    def on_ssos_query(self):
        try:
            new_workunit = self.model.get_current_workunit().query_ssos()
            self.model.add_workunit(new_workunit)
        except AssertionError as e:
            logger.critical(str(e))
            pass
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

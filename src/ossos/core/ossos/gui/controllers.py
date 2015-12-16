from astropy import units
from astropy.units import Quantity

from ..downloads.cutouts.source import SourceCutout
from ..gui.models.transactions import TransAckValidationModel
from ..gui.models.workload import TracksWorkUnit
from ..gui.views.appview import ApplicationView

__author__ = "David Rusk <drusk@uvic.ca>"
import math
from ..downloads.core import Downloader
from ..gui.autoplay import AutoplayManager
from ..gui import config, logger
from ..gui import events
from ..gui.models.exceptions import (ImageNotLoadedException,
                                     NoWorkUnitException)
from .. import mpc
from ..orbfit import OrbfitError, Orbfit


class AbstractController(object):
    """
    The class that handles interactions between the data model and the users view of that data.
    """
    def __init__(self, model, view):
        """

        @param model: The model that handles getting the data.
        @type model: TransAckValidationModel
        @param view: the display that is showing the controller interaction
        @type view: ApplicationView
        """
        self.model = model
        self.view = view
        events.subscribe(events.CHANGE_IMAGE, self.on_change_image)
        events.subscribe(events.IMG_LOADED, self.on_image_loaded)
        events.subscribe(events.NO_AVAILABLE_WORK, self.on_no_available_work)
        self.mark_prediction = False
        self.mark_source = True
        self.use_pixel_coords = True
        self.autoplay_manager = AutoplayManager(model)
        self.downloader = Downloader()
        self.image_loading_dialog_manager = ImageLoadingDialogManager(view)
        self.align = True

    def get_view(self):
        """
        @rtype: ApplicationView
        """
        return self.view

    def place_marker(self, x, y, radius=10, colour='r'):
        self.view.place_marker(self.model.get_current_cutout(), x, y, radius, colour=colour)

    def display_current_image(self):
        logger.debug("Displaying image.")
        try:
            cutout = self.model.get_current_cutout()
            source = self.model.get_current_source()
            reading = self.model.get_current_reading()
            self.view.image_viewer.mark_source = self.mark_source
            self.view.image_viewer.mark_prediction = self.mark_prediction
            self.view.display(cutout, self.use_pixel_coords)
            logger.debug("Align: {}".format(self.align))
            if self.align:
                self.view.align(cutout, reading, source)
            else:
                self.view.image_viewer.ds9.set("wcs align yes")
        except ImageNotLoadedException as ex:
            logger.info("Waiting to load image: {}".format(ex))
            self.image_loading_dialog_manager.wait_for_item(ex.requested_item)
            pass
        except NoWorkUnitException as ex:
            logger.debug("No work? {}".format(ex))
            return False
        except Exception as ex:
            logger.error("Exception in get_current_cutout: {}".format(ex))
            return False

        logger.debug("Displaying metadata.")
        try:
            self.view.update_displayed_data(self.model.get_reading_data(),
                                            self.model.get_header_data_list())
        except Exception as ex:
            logger.error(type(ex))
            logger.error(str(ex))
            logger.error("Failed to get header for {}".format(self.model.get_current_reading()))
            pass

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
        logger.debug("Change Image Event: {}".format(event))
        if self.model.is_current_item_processed():
            self.view.disable_source_validation()
        else:
            self.view.enable_source_validation()

        self.display_current_image()

    def on_no_available_work(self, event):
        logger.debug("Available work event: {}".format(event))
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

    def on_load_comparison(self, research=False):
        raise NotImplementedError()

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

    def on_toggle_align(self):
        self.align = not self.align
        print self.align and "DS9 Focus Follow On" or "DS9 Focus Follow Off"
        logger.debug("Setting align to : {}".format(self.align))

    def on_reset_colormap(self):
        self.view.reset_colormap()


class ProcessRealsController(AbstractController):
    """
    The main controller of the process reals task.  Sets up the view and
    handles user interactions.
    """

    def __init__(self, model, view, name_generator):
        assert isinstance(model, TransAckValidationModel)
        assert isinstance(view, ApplicationView)
        super(ProcessRealsController, self).__init__(model, view)
        self.name_generator = name_generator
        self.is_discovery = True

    def _generate_provisional_name(self):
        return self.name_generator.generate_name(
            self.model.get_current_astrom_header(),
            self.model.get_current_fits_header())

    def on_load_comparison(self, research=False):
        pass

    def on_accept(self, auto=False):
        """
        Initiates acceptance procedure, gathering required data.
        """
        if self.model.is_current_source_named():
            provisional_name = self.model.get_current_source_name()
        else:
            provisional_name = self._generate_provisional_name()

        band = self.model.get_current_band()
        logger.debug("Got band {} and provisional_name {}".format(band, provisional_name))
        default_comment = ""

        source_cutout = self.model.get_current_cutout()

        if not auto:
            result = self.view.ds9.get('imexam key coordinate wcs fk5 degrees')
            # result = display.get("""imexam key coordinate $x $y $filename""")
            values = result.split()
            ra = Quantity(float(values[1]), unit=units.degree)
            dec = Quantity(float(values[2]), unit=units.degree)
            self.place_marker(ra, dec, radius=2 * units.arcsec, colour='green')
            key = values[0]
        else:
            key = isinstance(auto, bool) and " " or auto
            ra = source_cutout.ra
            dec = source_cutout.dec

        (x, y, extno) = source_cutout.world2pix(ra, dec, usepv=False)
        source_cutout.update_pixel_location((float(x), float(y)), extno)

        pre_daophot_pixel_x = source_cutout.pixel_x
        pre_daophot_pixel_y = source_cutout.pixel_y

        # self.view.mark_apertures(source_cutout, pixel=False)

        try:
            phot = self.model.get_current_source_observed_magnitude()
            cen_x = phot['XCENTER'][0]
            cen_y = phot['YCENTER'][0]
            obs_mag = phot['MAG'][0]
            obs_mag_err = phot['MERR'][0]
            phot_failure = phot['PIER'][0] != 0
            sky_failure = phot['SIER'][0] != 0
            cen_failure = phot['CIER'][0] != 0
        except Exception as er:
            logger.critical("PHOT ERROR: {}".format(er))
            phot_failure = sky_failure = cen_failure = True
            obs_mag = ""
            cen_x = pre_daophot_pixel_x
            cen_y = pre_daophot_pixel_y
            obs_mag_err = -1
            band = ""
            default_comment = str(er)

        obs_mag = phot_failure and None or obs_mag
        obs_mag_err = phot_failure and None or obs_mag_err
        source_cutout.update_pixel_location((cen_x, cen_y), extno=extno)
        source_cutout._adjusted = False

        if math.sqrt((cen_x - pre_daophot_pixel_x) ** 2 + (cen_y - pre_daophot_pixel_y) ** 2) > 1.5 or cen_failure:
            # check if the user wants to use the 'hand' coordinates or these new ones.
            self.place_marker(cen_x, cen_y, 6, colour='white')
            self.view.show_offset_source_dialog((pre_daophot_pixel_x, pre_daophot_pixel_y), (cen_x, cen_y))

        note1_default = ""
        if self.model.is_current_source_adjusted():
            note1_default = config.read("MPC.NOTE1_HAND_ADJUSTED")
        else:
            for note in config.read("MPC.NOTE1OPTIONS"):
                if note.lower().startswith(key):
                    note1_default = note
                    break
        note1 = len(note1_default) > 0 and note1_default[0] or note1_default

        if isinstance(self, ProcessTracksController):
            this_observation = mpc.Observation(
                null_observation=False,
                provisional_name=provisional_name,
                note1=note1,
                note2=config.read('MPC.NOTE2DEFAULT')[0],
                date=self.model.get_current_observation_date(),
                ra=self.model.get_current_ra(),
                dec=self.model.get_current_dec(),
                mag=obs_mag,
                mag_err=obs_mag_err,
                band=band,
                observatory_code=config.read("MPC.DEFAULT_OBSERVATORY_CODE"),
                discovery=self.is_discovery,
                comment=None,
                xpos=source_cutout.observed_x,
                ypos=source_cutout.observed_y,
                frame=self.model.get_current_reading().obs.rawname,
                astrometric_level=source_cutout.astrom_header.get('ASTLEVEL', None))

            try:
                previous_observations = self.model.get_writer().get_chronological_buffered_observations()
                for idx, observation in enumerate(previous_observations):
                    try:
                        if observation.comment.frame.strip() == this_observation.comment.frame.strip():
                            previous_observations[idx] = this_observation
                            this_observation = False
                            break
                    except Exception as ex:
                        print type(ex), str(ex)
                if this_observation:
                    previous_observations.append(this_observation)
                print Orbfit(previous_observations).summarize()
            except Exception as ex:
                logger.error(type(ex), str(ex))
                print "Failed to compute preliminary orbit."

        if obs_mag < 24 and auto is not False:
            self.on_do_accept(None,
                              provisional_name,
                              sky_failure and "S  poor sky" or note1_default,
                              config.read("MPC.NOTE2DEFAULT"),
                              self.model.get_current_observation_date(),
                              self.model.get_current_ra(),
                              self.model.get_current_dec(),
                              obs_mag,
                              obs_mag_err,
                              band,
                              config.read("MPC.DEFAULT_OBSERVATORY_CODE"), ""
                              )
        else:
            self.view.show_accept_source_dialog(
                provisional_name,
                self.model.get_current_observation_date(),
                self.model.get_current_ra(),
                self.model.get_current_dec(),
                obs_mag,
                obs_mag_err,
                band,
                note1_choices=config.read("MPC.NOTE1OPTIONS"),
                note2_choices=config.read("MPC.NOTE2OPTIONS"),
                note1_default=sky_failure and "S  poor sky" or note1_default,
                note2_default=config.read("MPC.NOTE2DEFAULT"),
                default_observatory_code=config.read("MPC.DEFAULT_OBSERVATORY_CODE"),
                default_comment=default_comment,
                phot_failure=phot_failure,
                pixel_x=source_cutout.pixel_x,
                pixel_y=source_cutout.pixel_y)

    def on_do_accept(self,
                     minor_planet_number,
                     provisional_name,
                     note1,
                     note2,
                     date_of_obs,
                     ra,
                     dec,
                     obs_mag,
                     obs_mag_err,
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

        reading = self.model.get_current_reading()
        source_cutout = self.model.get_current_cutout()

        mpc_observation = mpc.Observation(
            null_observation=False,
            provisional_name=provisional_name,
            note1=note1_code,
            note2=note2_code,
            date=date_of_obs,
            ra=ra,
            dec=dec,
            mag=obs_mag,
            mag_err=obs_mag_err,
            band=band,
            observatory_code=observatory_code,
            discovery=self.is_discovery,
            comment=comment,
            xpos=source_cutout.observed_x,
            ypos=source_cutout.observed_y,
            frame=reading.obs.rawname,
            astrometric_level=source_cutout.astrom_header.get('ASTLEVEL', None)
        )
        mpc_observation._date_precision = 6

        source_reading = self.model.get_current_reading()
        source_reading.sky_coord = mpc_observation.coordinate
        source_reading.pix_coord = mpc_observation.comment.x, mpc_observation.comment.y

        data = self.model.get_current_workunit().data
        key = mpc_observation.comment.frame.strip()
        data.mpc_observations[key] = mpc_observation

        self.model.get_writer().write(mpc_observation)

        self.model.accept_current_item()
        reset_frame = False
        if self.model.get_current_workunit().get_current_source_readings().is_on_last_item():
            self.view.clear()
            reset_frame = True
        self.model.next_item()
        if reset_frame:
            self.view.frame(1)

    def on_cancel_accept(self):
        self.view.close_accept_source_dialog()

    def on_reject(self):
        self.view.show_reject_source_dialog()

    def on_do_offset(self, cen_coords):
        self.view.close_offset_source_dialog()

        source_cutout = self.model.get_current_cutout()
        assert isinstance(source_cutout, SourceCutout)
        source_cutout.update_pixel_location(cen_coords, source_cutout.original_observed_ext)
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
        mpc_observation._date_precision = 6

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
        self.view.clear()
        self.model.next_item()

    def on_reject(self):
        self.model.reject_current_item()
        self.view.clear()
        self.model.next_item()

    def on_load_comparison(self, research=False):
        pass


class ProcessTracksController(ProcessRealsController):
    """
    The main controller of the 'track' task.  Sets up the view and
    handles user interactions. This task extends orbit linkages from
    three out to more observations.
    """

    def __init__(self, model, view, name_generator):
        super(ProcessTracksController, self).__init__(model, view, name_generator)
        assert isinstance(model, TransAckValidationModel)
        assert isinstance(view, ApplicationView)
        self.use_pixel_coords = False
        self.mark_prediction = True
        self.is_discovery = False
        self.minimum_align_offset = 60


    def on_accept(self, auto=False):
        super(ProcessTracksController, self).on_accept(auto=False)

    def on_do_accept(self,
                     minor_planet_number,
                     provisional_name,
                     note1,
                     note2,
                     date_of_obs,
                     ra,
                     dec,
                     obs_mag,
                     obs_mag_err,
                     band,
                     observatory_code,
                     comment):
        super(ProcessTracksController, self).on_do_accept(
            minor_planet_number,
            provisional_name,
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
            logger.error("Orbfit Error: {0}".format(error))
        self.is_discovery = False

    def on_load_comparison(self, research=False):
        """
        Display the comparison image
        """

        logger.debug(str(research))
        cutout = self.model.get_current_cutout()
        if research or cutout.comparison_image is None:
            cutout.retrieve_comparison_image()
        if cutout.comparison_image is not None:  # if a comparison image was found
            self.view.display(cutout.comparison_image, self.use_pixel_coords)
            self.view.align(self.model.get_current_cutout(),
                            self.model.get_current_reading(),
                            self.model.get_current_source())
            self.model.get_current_workunit().previous_obs()
            self.model.acknowledge_image_displayed()

    def on_ssos(self):
        try:
            new_workunit = self.model.get_current_workunit().query_ssos()
            self.model.add_workunit(new_workunit)
        except AssertionError as e:
            logger.critical(str(e))
            pass
        self.model.next_item()

    def on_save(self):
        workunit = self.model.get_current_workunit()
        assert isinstance(workunit, TracksWorkUnit)
        print "Saved to: {}".format(workunit.save())


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

import logging
import time

from astropy import units

from ..astrom import SourceReading
from ..downloads.cutouts.source import SourceCutout
from ..gui import logger

__author__ = "David Rusk <drusk@uvic.ca>"


class WxMPLFitsViewer(object):
    """
    Display FITS images using ds9.
    """

    def __init__(self, parent, display):
        self.parent = parent

        self.current_cutout = None
        self.current_displayable = None
        self._ds9 = display
        self._displayables_by_cutout = {}
        self.mark_source = True
        self.mark_prediction = False

    @property
    def ds9(self):
        return self._ds9

    def display(self, cutout, use_pixel_coords=False):
        """
        :param cutout: source cutout object to be display
        :type cutout: source.SourceCutout
        """
        logging.debug("Current display list contains: {}".format(list(self._displayables_by_cutout.keys())))
        logging.debug("Looking for {}".format(cutout))
        assert isinstance(cutout, SourceCutout)
        if cutout in self._displayables_by_cutout:
            displayable = self._displayables_by_cutout[cutout]
        else:
            displayable = self._create_displayable(cutout)
            self._displayables_by_cutout[cutout] = displayable
        self._detach_handlers(self.current_displayable)
        self.current_cutout = cutout
        self.current_displayable = displayable
        self._attach_handlers(self.current_displayable)
        self._do_render(self.current_displayable)
        self.mark_apertures(cutout, pixel=use_pixel_coords)
        self.draw_uncertainty_ellipse(cutout)

    def clear(self):
        self.ds9.set("frame delete all")

    def draw_uncertainty_ellipse(self, cutout):
        """
        Draws an ErrEllipse with the spcified dimensions.  Only one ErrEllipse can be drawn and
        only once (not movable).
        """
        if not self.mark_prediction:
            return

        try:
            colour = cutout.reading.from_input_file and 'b' or 'g'
            colour = cutout.reading.null_observation and 'c' or colour
            dash = cutout.reading.null_observation and 1 or 0
            sky_coord = cutout.reading.sky_coord

            # RA/DEC value of observation / prediction goes to X/Y  using PV keywords
            predict_ra = sky_coord.ra.to(units.degree)
            predict_dec = sky_coord.dec.to(units.degree)
            x, y, extno = cutout.world2pix(predict_ra, predict_dec, usepv=True)

            # The X/Y pixel values are then converted to RA/DEC without PV for use with DS9.
            ra, dec = cutout.pix2world(x, y, extno, usepv=False)
            uncertainty_ellipse = cutout.reading.uncertainty_ellipse

            self.current_displayable.place_ellipse((ra, dec), uncertainty_ellipse, colour=colour, dash=dash)

            # Also mark on the image where source is localted, not just the ellipse.
            try:
                measured_ra = cutout.reading.mpc_observation.coordinate.ra.to(units.degree)
                measured_dec = cutout.reading.mpc_observation.coordinate.dec.to(units.degree)
                try:
                    print("{:5.2f} {:5.2f} || {:5.2f} {:5.2f} # {}".format((predict_ra - measured_ra).to('arcsec'),
                                                                           (predict_dec - measured_dec).to('arcsec'),
                                                                           cutout.reading.uncertainty_ellipse.a,
                                                                           cutout.reading.uncertainty_ellipse.b,
                                                                           cutout.reading.mpc_observation.to_string()))
                except Exception as ex:
                    print("Failed trying to write out the prevoiusly recorded measurement: {}".format(ex))
                    pass
                # x, y, extno = cutout.world2pix(measured_ra, measured_dec, usepv=True)
                # ra, dec = cutout.pix2world(x, y, extno, usepv=False)

            except Exception as ex:
                # print "We had an error :{}".format(ex)
                logging.debug(str(ex))
                logging.debug("Failed to get x/y from previous observation, using prediction.")
                hdulist_index = cutout.get_hdulist_idx(cutout.reading.get_ccd_num())
                measured_ra, measured_dec = cutout.pix2world(cutout.pixel_x, cutout.pixel_y, hdulist_index, usepv=True)
                colour = 'magenta'
            self.current_displayable.place_marker(measured_ra, measured_dec, radius=8, colour=colour)
        except Exception as ex:
            logger.error("EXCEPTION while drawing the uncertainty ellipse, skipping.: {}".format(ex))
            logger.debug(type(ex))
            logger.debug(str(ex))

    def refresh_markers(self):
        self.mark_apertures(self.current_cutout)

    def mark_apertures(self, cutout, pixel=False):
        pass

    def release_focus(self):
        self.parent.SetFocus()

    def reset_colormap(self):
        if self.current_displayable is not None:
            self.current_displayable.reset_colormap()

    def toggle_reticule(self):
        if self.current_displayable.toggle_reticule():
            self.mark_apertures(self.current_cutout, pixel=False)
            self.draw_uncertainty_ellipse(self.current_cutout)

    def _attach_handlers(self, displayable):
        pass

    def _detach_handlers(self, displayable):
        pass

    def _create_displayable(self, cutout):
        raise NotImplementedError()

    def align(self, cutout, reading, source):
        """
        Set the display center to the reference point.

        @param cutout:  The cutout to align on
        @type cutout: SourceCutout
        @param reading:  The reading this cutout is from
        @type reading: SourceReading
        @param source: The source the reading is from
        @type source: Source
        @return:
        """
        if not self.current_displayable:
            return
        if not self.current_displayable.aligned:
            focus_sky_coord = reading.reference_sky_coord
            #focus_calculator = SingletFocusCalculator(source)
            #logger.debug("Got focus calculator {} for source {}".format(focus_calculator, source))
            #focus = cutout.flip_flip(focus_calculator.calculate_focus(reading))
            #focus = cutout.get_pixel_coordinates(focus)
            #focus = cutout.pix2world(focus[0], focus[1], usepv=False)
            #focus_sky_coord = SkyCoord(focus[0], focus[1])
            self.current_displayable.pan_to(focus_sky_coord)

    @staticmethod
    def _do_render(displayable):
        displayable.render()

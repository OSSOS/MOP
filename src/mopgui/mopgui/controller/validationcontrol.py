__author__ = "David Rusk <drusk@uvic.ca>"

from wx.lib.pubsub import Publisher as pub

# Pub/Sub ids
MSG_ROOT = ("valctrlroot", )
MSG_INITIATE_ACCEPT = MSG_ROOT + ("initaccept", )
MSG_DO_ACCEPT = MSG_ROOT + ("doaccept", )
MSG_CANCEL_ACCEPT = MSG_ROOT + ("cancelaccept", )


class SourceValidationController(object):
    def __init__(self, model, output_writer, name_generator):
        self.model = model
        self.output_writer = output_writer
        self.name_generator = name_generator

    def _get_provisional_name(self):
        return self.name_generator.generate_name(
            self.model.get_current_exposure_number())

    def on_initiate_accept(self, event):
        """Initiates acceptance procedure, gathering required data."""
        preset_vals = (
            self._get_provisional_name(),
            self.model.get_current_observation_date(),
            self.model.get_current_ra(),
            self.model.get_current_dec(),
            ["A", "B"], # TODO read from config file
            ["C", "D", "E"]  # TODO read from config file
        )
        pub.sendMessage(MSG_INITIATE_ACCEPT, data=preset_vals)

    def on_reject(self, event):
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
        self.output_writer.write_line(
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
            observatory_code)

        pub.sendMessage(MSG_DO_ACCEPT)
        self.model.next_source()

    def on_cancel_accept(self):
        pub.sendMessage(MSG_CANCEL_ACCEPT)

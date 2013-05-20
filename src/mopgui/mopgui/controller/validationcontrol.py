__author__ = "David Rusk <drusk@uvic.ca>"


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
        self.output_writer.write_result(self._get_provisional_name(),
                                        "accepted")

    def on_reject(self, event):
        self.output_writer.write_result(self._get_provisional_name(),
                                        "rejected")

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

    def on_cancel_accept(self):
        pass


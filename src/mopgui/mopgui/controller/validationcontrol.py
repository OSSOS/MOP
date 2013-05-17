__author__ = "David Rusk <drusk@uvic.ca>"


class SourceValidationController(object):
    def __init__(self, model, output_writer, name_generator):
        self.model = model
        self.output_writer = output_writer
        self.name_generator = name_generator

    def _get_provisional_name(self):
        return self.name_generator.generate_name(
            self.model.get_current_exposure_number())

    def on_accept(self, event):
        self.output_writer.write_result(self._get_provisional_name(),
                                        "accepted")

    def on_reject(self, event):
        self.output_writer.write_result(self._get_provisional_name(),
                                        "rejected")

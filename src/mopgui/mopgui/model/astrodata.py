"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""


class AstroDataModel(object):
    """
    Main model for storing and accessing astronomical data in the
    application.
    """

    def __init__(self, astrom_data):
        self.astrom_data = astrom_data

        self._current_source_number = 0

    def get_current_source_number(self):
        return self._current_source_number

    def get_source_count(self):
        return len(self.astrom_data.sources)

    def next_source(self):
        self._current_source_number = (self._current_source_number + 1) % self.get_source_count()

    def previous_source(self):
        self._current_source_number = (self._current_source_number - 1) % self.get_source_count()


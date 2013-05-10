"""
Provides interfaces to the application data which can be manipulated by the
user interface.
"""

from wx.lib.pubsub import Publisher as pub

# Pub/Sub ids
MSG_ROOT = ("astrodataroot", )
MSG_NEXT_SRC = MSG_ROOT + ("nextsrc",)
MSG_PREV_SRC = MSG_ROOT + ("prevsrc",)
MSG_NEXT_OBS = MSG_ROOT + ("nextobs",)
MSG_PREV_OBS = MSG_ROOT + ("prevobs",)


class AstroDataModel(object):
    """
    Main model for storing and accessing astronomical data in the
    application.
    """

    def __init__(self, astrom_data):
        self.astrom_data = astrom_data

        self._current_src_number = 0
        self._current_obs_number = 0

    def get_current_source_number(self):
        return self._current_src_number

    def get_source_count(self):
        return len(self.astrom_data.sources)

    def next_source(self):
        self._current_src_number = (self._current_src_number + 1) % self.get_source_count()
        pub.sendMessage(MSG_NEXT_SRC, data=self._current_src_number)

    def previous_source(self):
        self._current_src_number = (self._current_src_number - 1) % self.get_source_count()
        pub.sendMessage(MSG_PREV_SRC, data=self._current_src_number)

    def get_current_obs_number(self):
        return self._current_obs_number

    def get_obs_count(self):
        return len(self._get_current_source())

    def next_obs(self):
        self._current_obs_number = (self._current_obs_number + 1) % self.get_obs_count()
        pub.sendMessage(MSG_NEXT_OBS, data=self._current_obs_number)

    def previous_obs(self):
        self._current_obs_number = (self._current_obs_number - 1) % self.get_obs_count()
        pub.sendMessage(MSG_PREV_OBS, data=self._current_obs_number)

    def _get_current_source(self):
        return self.astrom_data.sources[self._current_src_number]

    def _get_current_reading(self):
        return self._get_current_source()[self._current_obs_number]

    def get_source_readings(self):
        reading = self._get_current_reading()
        return (("X", reading.x), ("Y", reading.y), ("X_0", reading.x0),
                ("Y_0", reading.y0), ("R.A.", reading.ra),
                ("DEC", reading.dec))


import unittest

from wx.lib.pubsub import Publisher as pub

from hamcrest import assert_that, equal_to, has_length, contains
from mock import Mock

from test.base_tests import FileReadingTestCase
from mopgui.model import astrodata
from mopgui.io.parser import AstromParser


class AstroDataModelTest(FileReadingTestCase):
    def setUp(self):
        testfile = self.get_abs_path("data/1584431p15.measure3.cands.astrom")
        astrom_data = AstromParser().parse(testfile)

        self.model = astrodata.AstroDataModel(astrom_data)

    def test_sources_initialized(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_source_count(), equal_to(3))
        assert_that(self.model.get_obs_count(), equal_to(3))

    def test_next_source_previous_source(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))

    def test_next_source_wrap(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))

    def test_next_obs(self):
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        self.model.next_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        self.model.next_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(2))
        self.model.next_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(0))

    def test_previous_source_wrap(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))

    def test_previous_obs(self):
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        self.model.previous_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(2))
        self.model.previous_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        self.model.previous_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(0))

    def test_receive_next_source_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_next_event, astrodata.MSG_NEXT_SRC)

        # Perform action
        self.model.next_source()

        # Make sure event triggered
        observer.on_next_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_next_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(astrodata.MSG_NEXT_SRC))
        assert_that(msg.data, equal_to(1))

    def test_receive_next_obs_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_next_event, astrodata.MSG_NEXT_OBS)

        # Perform action
        self.model.next_obs()

        # Make sure event triggered
        observer.on_next_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_next_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(astrodata.MSG_NEXT_OBS))
        assert_that(msg.data, equal_to(1))

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_previous_event, astrodata.MSG_PREV_SRC)

        # Perform actions
        self.model.next_source()
        self.model.previous_source()

        # Make sure event triggered
        observer.on_previous_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_previous_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(astrodata.MSG_PREV_SRC))
        assert_that(msg.data, equal_to(0))

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_previous_event, astrodata.MSG_PREV_OBS)

        # Perform actions
        self.model.next_obs()
        self.model.previous_obs()

        # Make sure event triggered
        observer.on_previous_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_previous_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(astrodata.MSG_PREV_OBS))
        assert_that(msg.data, equal_to(0))

    def test_get_reading_data(self):
        assert_that(self.model.get_reading_data(), contains(
            ("X", 911.00), ("Y", 3967.12), ("X_0", 911.00), ("Y_0", 3967.12),
            ("R.A.", 26.6833367), ("DEC", 29.2203532)
        ))


if __name__ == '__main__':
    unittest.main()

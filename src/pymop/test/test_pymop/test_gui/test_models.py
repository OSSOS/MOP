__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from wx.lib.pubsub import Publisher as pub

from hamcrest import assert_that, equal_to, has_length, contains, none, same_instance
from mock import Mock, patch

from test.base_tests import FileReadingTestCase
from pymop.gui import models
from pymop.gui.models import VettableItem
from pymop.io.astrom import AstromParser
from pymop.io.img import FitsImage


class GeneralModelTest(FileReadingTestCase):
    def setUp(self):
        pub.unsubAll()

        testfile = self.get_abs_path("data/1584431p15.measure3.cands.astrom")
        self.astrom_data = AstromParser().parse(testfile)
        self.download_manager = Mock()

    def create_real_first_image(self, path="data/testimg.fits"):
        # Put a real fits image on the first source, first observation
        apcor_str = "4 15   0.19   0.01"
        with open(self.get_abs_path(path), "rb") as fh:
            self.first_image = FitsImage(fh.read(), apcor_str, Mock(), in_memory=True)
            self.astrom_data.sources[0].get_reading(0).set_fits_image(self.first_image)


class AbstractRealsModelTest(GeneralModelTest):
    def setUp(self):
        super(AbstractRealsModelTest, self).setUp()

        # TODO: just use AbstractModel?
        self.model = models.ProcessRealsModel(self.astrom_data, self.download_manager)

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
        pub.subscribe(observer.on_next_event, models.MSG_NEXT_SRC)

        # Perform action
        self.model.next_source()

        # Make sure event triggered
        observer.on_next_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_next_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(models.MSG_NEXT_SRC))
        assert_that(msg.data, equal_to(1))

    def test_receive_next_obs_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_next_event, models.MSG_NEXT_OBS)

        # Perform action
        self.model.next_obs()

        # Make sure event triggered
        observer.on_next_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_next_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(models.MSG_NEXT_OBS))
        assert_that(msg.data, equal_to(1))

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_previous_event, models.MSG_PREV_SRC)

        # Perform actions
        self.model.next_source()
        self.model.previous_source()

        # Make sure event triggered
        observer.on_previous_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_previous_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(models.MSG_PREV_SRC))
        assert_that(msg.data, equal_to(0))

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_previous_event, models.MSG_PREV_OBS)

        # Perform actions
        self.model.next_obs()
        self.model.previous_obs()

        # Make sure event triggered
        observer.on_previous_event.assert_called_once()

        # Make sure it was triggered with the right data
        args = observer.on_previous_event.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(models.MSG_PREV_OBS))
        assert_that(msg.data, equal_to(0))

    def test_receive_nav_event_next_and_prev_source(self):
        # Subscribe a mock
        observer = Mock()
        pub.subscribe(observer.on_nav, models.MSG_NAV)

        # Perform actions
        self.model.next_obs()
        self.model.previous_obs()

        # Make sure event triggered
        assert_that(observer.on_nav.call_count, equal_to(2))

    def test_get_reading_data(self):
        assert_that(self.model.get_reading_data(), contains(
            ("X", 911.00), ("Y", 3967.12), ("X_0", 911.00), ("Y_0", 3967.12),
            ("R.A.", 26.6833367), ("DEC", 29.2203532)
        ))

    def test_loading_images(self):
        observer = Mock()
        pub.subscribe(observer.on_img_loaded, models.MSG_IMG_LOADED)

        self.model.start_loading_images()

        assert_that(self.download_manager.start_download.call_count, equal_to(1))

        assert_that(self.model.get_loaded_image_count(), equal_to(0))

        # Simulate receiving callback
        self.model._on_image_loaded(0, 1)
        assert_that(self.model.get_loaded_image_count(), equal_to(1))
        assert_that(observer.on_img_loaded.call_count, equal_to(1))

        # Simulate receiving callback
        self.model._on_image_loaded(1, 1)
        assert_that(self.model.get_loaded_image_count(), equal_to(2))
        assert_that(observer.on_img_loaded.call_count, equal_to(2))

        # Check event args
        call_args_list = observer.on_img_loaded.call_args_list
        assert_that(call_args_list, has_length(2))

        msg0 = call_args_list[0][0][0]
        assert_that(msg0.topic, equal_to(models.MSG_IMG_LOADED))
        assert_that(msg0.data, equal_to((0, 1)))

        msg1 = call_args_list[1][0][0]
        assert_that(msg1.topic, equal_to(models.MSG_IMG_LOADED))
        assert_that(msg1.data, equal_to((1, 1)))

    def test_get_current_exposure_number(self):
        assert_that(self.model.get_current_exposure_number(), equal_to(1584431))
        self.model.next_obs()
        assert_that(self.model.get_current_exposure_number(), equal_to(1584449))
        self.model.next_obs()
        assert_that(self.model.get_current_exposure_number(), equal_to(1584453))
        self.model.next_obs()
        assert_that(self.model.get_current_exposure_number(), equal_to(1584431))
        self.model.previous_obs()
        assert_that(self.model.get_current_exposure_number(), equal_to(1584453))

    def test_get_current_data(self):
        assert_that(self.model.get_current_observation_date(), equal_to("2012 10 21.40516"))
        assert_that(self.model.get_current_ra(), equal_to(26.6833367))
        assert_that(self.model.get_current_dec(), equal_to(29.2203532))

        self.model.next_obs()

        assert_that(self.model.get_current_observation_date(), equal_to("2012 10 21.48212"))
        assert_that(self.model.get_current_ra(), equal_to(26.6816808))
        assert_that(self.model.get_current_dec(), equal_to(29.2202748))

        self.model.next_source()

        assert_that(self.model.get_current_observation_date(), equal_to("2012 10 21.48212"))
        assert_that(self.model.get_current_ra(), equal_to(26.6816808))
        assert_that(self.model.get_current_dec(), equal_to(29.2202748))

    def test_get_total_image_count(self):
        assert_that(self.model.get_total_image_count(), equal_to(9))

    def test_sources_processed(self):
        assert_that(self.model.get_num_items_processed(), equal_to(0))
        assert_that(self.model.get_current_source_number(), equal_to(0))

        self.model.accept_current_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_num_items_processed(), equal_to(1))
        assert_that(self.model.is_item_processed(self.astrom_data.sources[0].get_reading(0)))
        assert_that(not self.model.is_item_processed(self.astrom_data.sources[0].get_reading(1)))
        assert_that(not self.model.is_item_processed(self.astrom_data.sources[0].get_reading(2)))

        self.model.reject_current_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_num_items_processed(), equal_to(1))

        self.model.next_item()
        self.model.reject_current_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_num_items_processed(), equal_to(2))

    def test_get_current_band(self):
        self.create_real_first_image("data/1616681p22.fits")
        assert_that(self.model.get_current_band(), equal_to("r"))

    def test_get_current_image(self):
        self.create_real_first_image()
        assert_that(self.model.get_current_image(),
                    same_instance(self.first_image))

    @patch("pymop.astrometry.daophot.phot_mag")
    def test_get_current_source_observed_magnitude(self, mock_phot_mag):
        first_image = Mock()
        self.astrom_data.sources[0].get_reading(0).set_fits_image(first_image)

        x, y = (1500, 2500)
        self.model.get_current_image_source_point = Mock(return_value=(x, y))

        self.model.get_current_source_observed_magnitude()

        first_image.get_observed_magnitude.assert_called_once_with(x, y,
                                                                   maxcount=30000.0)

    def test_get_current_hdulist_is_none(self):
        self.model.next_source()
        assert_that(self.model.get_current_hdulist(), none())

    def test_get_current_reading_data(self):
        self.model.next_source()
        self.model.next_obs()

        assert_that(self.model.get_current_ra(), equal_to(26.6816808))
        assert_that(self.model.get_current_dec(), equal_to(29.2202748))
        assert_that(self.model.get_current_image_FWHM(), equal_to(3.30))
        assert_that(self.model.get_current_image_maxcount(), equal_to(30000.0))

    def test_receive_all_sources_processed_event_on_final_accept(self):
        observer = Mock()
        pub.subscribe(observer.on_all_processed, models.MSG_ALL_ITEMS_PROC)

        item = 0
        while item < self.model.get_item_count() - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.accept_current_item()
        assert_that(observer.on_all_processed.call_count, equal_to(1))

    def test_receive_all_sources_processed_event_on_final_reject(self):
        observer = Mock()
        pub.subscribe(observer.on_all_processed, models.MSG_ALL_ITEMS_PROC)

        item = 0
        while item < self.model.get_item_count() - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.reject_current_item()
        assert_that(observer.on_all_processed.call_count, equal_to(1))


class ProcessRealsModelTest(GeneralModelTest):
    def setUp(self):
        super(ProcessRealsModelTest, self).setUp()

        self.model = models.ProcessRealsModel(self.astrom_data, self.download_manager)

    def test_next_item(self):
        observer = Mock()
        pub.subscribe(observer.on_next_obs, models.MSG_NEXT_OBS)
        pub.subscribe(observer.on_next_src, models.MSG_NEXT_SRC)

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        assert_that(observer.on_next_obs.call_count, equal_to(1))
        assert_that(observer.on_next_src.call_count, equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))
        assert_that(observer.on_next_obs.call_count, equal_to(2))
        assert_that(observer.on_next_src.call_count, equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        assert_that(observer.on_next_obs.call_count, equal_to(2))
        assert_that(observer.on_next_src.call_count, equal_to(1))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        assert_that(observer.on_next_obs.call_count, equal_to(3))
        assert_that(observer.on_next_src.call_count, equal_to(1))

    def test_is_source_discovered(self):
        assert_that(self.model.is_current_source_discovered(), equal_to(False))
        self.model.reject_current_item()
        assert_that(self.model.is_current_source_discovered(), equal_to(False))
        self.model.next_item()
        assert_that(self.model.is_current_source_discovered(), equal_to(False))
        self.model.accept_current_item()
        assert_that(self.model.is_current_source_discovered(), equal_to(True))
        self.model.next_item()
        assert_that(self.model.is_current_source_discovered(), equal_to(True))
        self.model.accept_current_item()
        assert_that(self.model.is_current_source_discovered(), equal_to(True))
        self.model.next_source()
        assert_that(self.model.is_current_source_discovered(), equal_to(False))

    def test_accept_current_item(self):
        first_item = self.astrom_data.sources[0].get_reading(0)
        second_item = self.astrom_data.sources[0].get_reading(1)

        assert_that(self.model.is_item_processed(first_item), equal_to(False))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.UNPROCESSED))
        assert_that(self.model.is_item_processed(second_item), equal_to(False))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.UNPROCESSED))

        self.model.accept_current_item()

        assert_that(self.model.is_item_processed(first_item), equal_to(True))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.ACCEPTED))
        assert_that(self.model.is_item_processed(second_item), equal_to(False))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.UNPROCESSED))

        self.model.next_item()
        self.model.accept_current_item()

        assert_that(self.model.is_item_processed(first_item), equal_to(True))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.ACCEPTED))
        assert_that(self.model.is_item_processed(second_item), equal_to(True))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.ACCEPTED))

    def test_reject_current_item(self):
        first_item = self.astrom_data.sources[0].get_reading(0)
        second_item = self.astrom_data.sources[0].get_reading(1)

        assert_that(self.model.is_item_processed(first_item), equal_to(False))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.UNPROCESSED))
        assert_that(self.model.is_item_processed(second_item), equal_to(False))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.UNPROCESSED))

        self.model.reject_current_item()

        assert_that(self.model.is_item_processed(first_item), equal_to(True))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.REJECTED))
        assert_that(self.model.is_item_processed(second_item), equal_to(False))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.UNPROCESSED))

        self.model.next_item()
        self.model.reject_current_item()

        assert_that(self.model.is_item_processed(first_item), equal_to(True))
        assert_that(self.model.get_item_status(first_item), equal_to(VettableItem.REJECTED))
        assert_that(self.model.is_item_processed(second_item), equal_to(True))
        assert_that(self.model.get_item_status(second_item), equal_to(VettableItem.REJECTED))


if __name__ == '__main__':
    unittest.main()

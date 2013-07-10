__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import (assert_that, equal_to, has_length, contains, same_instance, is_not, contains_inanyorder)
from mock import Mock, patch

from tests.base_tests import FileReadingTestCase, DirectoryCleaningTestCase
from ossos.gui.app import DirectoryContext
from ossos.gui import models, events, tasks
from ossos.gui.models import ImageNotLoadedException
from ossos.gui.downloads import AsynchronousImageDownloadManager
from ossos.astrom import AstromParser
from ossos.gui.persistence import ProgressManager
from ossos.gui.image import DownloadedFitsImage
from ossos.gui.workload import (WorkUnitProvider, RealsWorkUnitBuilder,
                               CandidatesWorkUnitBuilder)

MODEL_TEST_DIR_1 = "data/model_testdir_1"
MODEL_TEST_DIR_2 = "data/model_testdir_2"
MODEL_TEST_DIR_3 = "data/model_testdir_3"

FRESH_TEST_DIR = "data/model_persistence_fresh"
TEST_FILES = ["xxx1.cands.astrom", "xxx2.cands.astrom", "xxx3.reals.astrom", "xxx4.reals.astrom"]


class GeneralModelTest(FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        events.unsub_all()

        parser = AstromParser()
        directory_manager = DirectoryContext(self._get_working_dir())
        progress_manager = ProgressManager(directory_manager)
        workunit_provider = WorkUnitProvider(tasks.get_suffix(self._get_task()),
                                             directory_manager, progress_manager,
                                             self._get_workunit_builder(parser, progress_manager))

        self.workunit_provider = workunit_provider
        self.progress_manager = progress_manager
        self.download_manager = Mock(spec=AsynchronousImageDownloadManager)

        self.model = self.get_model()

    def get_model(self):
        raise NotImplementedError()

    def _get_task(self):
        raise NotImplementedError()

    def _get_working_dir(self):
        raise NotImplementedError()

    def get_directory_to_clean(self):
        return self._get_working_dir()

    def _get_workunit_builder(self, parser, progress_manager):
        raise NotImplementedError()

    def create_real_first_image(self, path="data/testimg.fits"):
        # Put a real fits image on the first source, first observation
        apcor_str = "4 15   0.19   0.01"
        with open(self.get_abs_path(path), "rb") as fh:
            self.first_image = DownloadedFitsImage(fh.read(), Mock(), apcor_str, in_memory=True)
            first_reading = self.model.get_current_workunit().get_sources()[0].get_readings()[0]
            self.model._on_image_loaded(first_reading, self.first_image)


class AbstractRealsModelTest(GeneralModelTest):
    def _get_working_dir(self):
        return self.get_abs_path(MODEL_TEST_DIR_1)

    def _get_task(self):
        return tasks.REALS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def get_files_to_keep(self):
        return ["1584431p15.measure3.reals.astrom"]

    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def test_sources_initialized(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
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

    def test_next_source_resets_obs(self):
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        self.model.next_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        self.model.next_source()
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        self.model.next_obs()
        assert_that(self.model.get_current_obs_number(), equal_to(1))

    def test_receive_next_source_event(self):
        # Subscribe a mock
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_next_event)

        # Perform action
        self.model.next_source()

        # Make sure event triggered
        observer.on_next_event.assert_called_once()

    def test_receive_next_obs_event(self):
        # Subscribe a mock
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_change_img)

        # Perform action
        self.model.next_obs()

        # Make sure event triggered
        observer.on_change_img.assert_called_once()

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_change_img)

        # Perform actions
        self.model.next_source()
        self.model.previous_source()

        # Make sure event triggered
        observer.on_change_img.assert_called_once()

    def test_receive_previous_source_event(self):
        # Subscribe a mock
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_change_img)

        # Perform actions
        self.model.next_obs()
        self.model.previous_obs()

        # Make sure event triggered
        observer.on_change_img.assert_called_once()

    def test_receive_nav_event_next_and_prev_source(self):
        # Subscribe a mock
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_nav)

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
        events.subscribe(events.IMG_LOADED, observer.on_img_loaded)
        loaded_reading1 = Mock()
        image1 = Mock()
        loaded_reading2 = Mock()
        image2 = Mock()

        assert_that(self.download_manager.start_downloading_workunit.call_count,
                    equal_to(1))
        assert_that(self.model.get_loaded_image_count(), equal_to(0))

        # Simulate receiving callback
        self.model._on_image_loaded(loaded_reading1, image1)
        assert_that(self.model.get_loaded_image_count(), equal_to(1))
        assert_that(observer.on_img_loaded.call_count, equal_to(1))

        # Simulate receiving callback
        self.model._on_image_loaded(loaded_reading2, image2)
        assert_that(self.model.get_loaded_image_count(), equal_to(2))
        assert_that(observer.on_img_loaded.call_count, equal_to(2))

        # Check event args
        call_args_list = observer.on_img_loaded.call_args_list
        assert_that(call_args_list, has_length(2))

        msg0 = call_args_list[0][0][0]
        assert_that(msg0.topic, equal_to(events.IMG_LOADED))
        assert_that(msg0.data, equal_to(loaded_reading1))

        msg1 = call_args_list[1][0][0]
        assert_that(msg1.topic, equal_to(events.IMG_LOADED))
        assert_that(msg1.data, equal_to(loaded_reading2))

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
        self.model.next_obs()

        assert_that(self.model.get_current_observation_date(), equal_to("2012 10 21.48212"))
        assert_that(self.model.get_current_ra(), equal_to(26.6816808))
        assert_that(self.model.get_current_dec(), equal_to(29.2202748))

    def test_sources_processed(self):
        workunit = self.model.get_current_workunit()

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        assert_that(self.model.get_current_source_number(), equal_to(0))

        self.model.accept_current_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_num_items_processed(), equal_to(1))

        source1 = self.model.get_current_data().get_sources()[0]
        assert_that(workunit.is_item_processed(source1.get_reading(0)), equal_to(True))
        assert_that(workunit.is_item_processed(source1.get_reading(1)), equal_to(False))
        assert_that(workunit.is_item_processed(source1.get_reading(2)), equal_to(False))

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

    @patch("ossos.daophot.phot_mag")
    def test_get_current_source_observed_magnitude(self, mock_phot_mag):
        first_image = Mock()
        first_reading = self.model.get_current_data().get_sources()[0].get_readings()[0]
        self.model._on_image_loaded(first_reading, first_image)

        x, y = (1500, 2500)
        self.model.get_current_image_source_point = Mock(return_value=(x, y))

        self.model.get_current_source_observed_magnitude()

        first_image.get_observed_magnitude.assert_called_once_with(x, y,
                                                                   maxcount=30000.0)

    def test_get_current_hdulist_image_not_loaded(self):
        self.model.next_source()
        self.assertRaises(ImageNotLoadedException, self.model.get_current_hdulist)

    def test_get_current_reading_data(self):
        self.model.next_source()
        self.model.next_obs()

        assert_that(self.model.get_current_ra(), equal_to(26.6816808))
        assert_that(self.model.get_current_dec(), equal_to(29.2202748))
        assert_that(self.model.get_current_image_FWHM(), equal_to(3.30))
        assert_that(self.model.get_current_image_maxcount(), equal_to(30000.0))


class ProcessRealsModelTest(GeneralModelTest):
    def _get_working_dir(self):
        return self.get_abs_path(MODEL_TEST_DIR_1)

    def _get_task(self):
        return tasks.REALS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def get_files_to_keep(self):
        return ["1584431p15.measure3.reals.astrom"]

    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def test_next_item_no_validation(self):
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_change_img)

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))
        assert_that(observer.on_change_img.call_count, equal_to(1))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))
        assert_that(observer.on_change_img.call_count, equal_to(2))

        self.model.next_item()
        # Should have looped back to first observation of the same source
        # because we haven't finished processing it.
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        assert_that(observer.on_change_img.call_count, equal_to(3))

    def test_next_item_after_validate_last(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

        self.model.accept_current_item()
        self.model.next_item()

        # Should have looped back to first observation of the same source
        # because we haven't finished processing it.
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.accept_current_item()
        self.model.next_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.model.accept_current_item()
        self.model.next_item()

        # We already validated the last reading, so we should be jumping
        # straight to the second source now.

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

    def test_next_item_jump_over_processed(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(1))

        self.model.reject_current_item()
        self.model.next_item()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(2))

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
        workunit = self.model.get_current_workunit()

        data = self.model.get_current_data()
        first_item = data.get_sources()[0].get_readings()[0]
        second_item = data.get_sources()[0].get_readings()[1]

        assert_that(workunit.is_item_processed(first_item), equal_to(False))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.accept_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.next_item()
        self.model.accept_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(True))

    def test_reject_current_item(self):
        workunit = self.model.get_current_workunit()

        data = self.model.get_current_data()
        first_item = data.get_sources()[0].get_readings()[0]
        second_item = data.get_sources()[0].get_readings()[1]

        assert_that(workunit.is_item_processed(first_item), equal_to(False))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.reject_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.next_item()
        self.model.reject_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(True))

    def test_receive_all_sources_processed_event_on_final_accept(self):
        observer = Mock()
        events.subscribe(events.NO_AVAILABLE_WORK, observer.on_all_processed)

        total_items = 9
        item = 0
        while item < total_items - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.accept_current_item()
        # We try to get the next work unit, and find there is none
        self.model.next_item()

        assert_that(observer.on_all_processed.call_count, equal_to(1))

    def test_receive_all_sources_processed_event_on_final_reject(self):
        observer = Mock()
        events.subscribe(events.NO_AVAILABLE_WORK, observer.on_all_processed)

        total_items = 9
        item = 0
        while item < total_items - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.reject_current_item()

        # We try to get the next work unit, and find there is none
        self.model.next_item()
        assert_that(observer.on_all_processed.call_count, equal_to(1))


class ProcessCandidatesModelTest(GeneralModelTest):
    def _get_working_dir(self):
        return self.get_abs_path(MODEL_TEST_DIR_3)

    def _get_task(self):
        return tasks.CANDS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

    def get_files_to_keep(self):
        return ["1584431p15.measure3.cands.astrom", "1584431p15.measure3.reals.astrom"]

    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def test_next_item(self):
        observer = Mock()
        events.subscribe(events.CHANGE_IMAGE, observer.on_change_img)

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        assert_that(observer.on_change_img.call_count, equal_to(1))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        assert_that(observer.on_change_img.call_count, equal_to(2))

        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        assert_that(observer.on_change_img.call_count, equal_to(3))

    def test_accept_current_item(self):
        workunit = self.model.get_current_workunit()

        data = self.model.get_current_data()
        first_item = data.get_sources()[0]
        second_item = data.get_sources()[1]

        assert_that(workunit.is_item_processed(first_item), equal_to(False))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.accept_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.next_item()
        self.model.accept_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(True))

    def test_reject_current_item(self):
        workunit = self.model.get_current_workunit()

        data = self.model.get_current_data()
        first_item = data.get_sources()[0]
        second_item = data.get_sources()[1]

        assert_that(workunit.is_item_processed(first_item), equal_to(False))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.reject_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(False))

        self.model.next_item()
        self.model.reject_current_item()

        assert_that(workunit.is_item_processed(first_item), equal_to(True))
        assert_that(workunit.is_item_processed(second_item), equal_to(True))

    def test_receive_all_sources_processed_event_on_final_accept(self):
        observer = Mock()
        events.subscribe(events.NO_AVAILABLE_WORK, observer.on_all_processed)

        total_items = 3
        item = 0
        while item < total_items - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.accept_current_item()

        # We try to get the next work unit, and find there is none
        self.model.next_item()
        assert_that(observer.on_all_processed.call_count, equal_to(1))

    def test_receive_all_sources_processed_event_on_final_reject(self):
        observer = Mock()
        events.subscribe(events.NO_AVAILABLE_WORK, observer.on_all_processed)

        total_items = 3
        item = 0
        while item < total_items - 1:
            self.model.accept_current_item()
            assert_that(observer.on_all_processed.call_count, equal_to(0))
            self.model.next_item()
            item += 1

        self.model.reject_current_item()

        # we try to get the next work unit, and find there is none
        self.model.next_item()
        assert_that(observer.on_all_processed.call_count, equal_to(1))


class MultipleAstromDataModelTest(GeneralModelTest):
    def _get_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def _get_task(self):
        return tasks.CANDS_TASK

    def _get_working_dir(self):
        return self.get_abs_path(MODEL_TEST_DIR_2)

    def get_files_to_keep(self):
        return ["1584431p15.measure3.cands.astrom", "1616681p10.measure3.cands.astrom"]

    def test_meta_data(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_obs_count(), equal_to(3)) # for the current data only

    def test_next_source(self):
        first_sources = self.model.get_current_data().get_sources()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(first_sources[0]))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_source(), equal_to(first_sources[1]))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_source(), equal_to(first_sources[2]))
        self.model.next_source()

        # Note we don't move to next file.  It is not loaded yet since we
        # have not processed the current one.
        assert_that(self.model.get_current_source_number(), equal_to(0))

        self.model.accept_current_item()
        self.model.next_item()
        self.model.accept_current_item()
        self.model.next_item()
        self.model.accept_current_item()

        # Moving to next work unit
        self.model.next_item()

        second_sources = self.model.get_current_data().get_sources()

        assert_that(first_sources, is_not(same_instance(second_sources)))

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(second_sources[0]))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_source(), equal_to(second_sources[1]))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_source(), equal_to(second_sources[2]))
        self.model.next_source()

        # Note we only iterate within the active work unit
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(second_sources[0]))

    def test_previous_source(self):
        first_sources = self.model.get_current_data().get_sources()

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(first_sources[0]))
        self.model.previous_source()

        # Note we don't move to next file.  It is not loaded yet since we
        # have not processed the current one.
        assert_that(self.model.get_current_source_number(), equal_to(2))
        self.model.accept_current_item()
        self.model.previous_source()
        self.model.accept_current_item()
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.accept_current_item()

        # Moving to next work unit
        self.model.next_item()

        second_sources = self.model.get_current_data().get_sources()
        assert_that(first_sources, is_not(same_instance(second_sources)))

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(second_sources[0]))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_source(), equal_to(second_sources[2]))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_source(), equal_to(second_sources[1]))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_source(), equal_to(second_sources[0]))
        self.model.previous_source()

        # Note we only iterate in the current work unit
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_source(), equal_to(second_sources[2]))

    def test_move_to_first_observation_of_new_workunit(self):
        workunit1 = self.model.get_current_workunit()
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.next_workunit()

        workunit2 = self.model.get_current_workunit()
        assert_that(workunit2, is_not(same_instance(workunit1)))

        assert_that(self.model.get_current_obs_number(), equal_to(0))

    def test_accept_moves_to_first_observation_of_new_workunit(self):
        workunit1 = self.model.get_current_workunit()
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.accept_current_item()
        self.model.next_item()

        workunit2 = self.model.get_current_workunit()
        assert_that(workunit2, is_not(same_instance(workunit1)))

        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_current_obs_number(), equal_to(0))


class RealsModelPersistenceTest(GeneralModelTest):
    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def setUp(self):
        super(RealsModelPersistenceTest, self).setUp()

        concurrent_directory_manager = DirectoryContext(self._get_working_dir())
        self.concurrent_progress_manager = ProgressManager(concurrent_directory_manager)

    def _get_task(self):
        return tasks.REALS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def _get_working_dir(self):
        return self.get_abs_path(FRESH_TEST_DIR)

    def get_directory_to_clean(self):
        return self._get_working_dir()

    def get_files_to_keep(self):
        return TEST_FILES

    def test_record_progress_only_after_source_finished(self):
        first_file = self.model.get_current_filename()

        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    has_length(0))

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    has_length(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    has_length(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

    def test_record_progress_multiple_sources(self):
        first_file = self.model.get_current_filename()

        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    has_length(0))

        # source 1 of 3, reading 1 of 3
        self.model.accept_current_item()
        self.model.next_item()

        # source 1 of 3 reading 2 of 3
        self.model.accept_current_item()
        self.model.next_item()

        # source 1 of 3 reading 3 of 3
        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        # source 2 of 3 reading 1 of 3
        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        # source 2 of 3 reading 2 of 3
        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        # source 2 of 3 reading 3 of 3
        self.model.accept_current_item()
        self.model.next_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1))

        # source 3 of 3 reading 1 of 3
        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1))

        # source 3 of 3 reading 2 of 3
        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1))

        # source 3 of 3 reading 3 of 3
        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(True))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1, 2))

    def test_file_processed_event(self):
        observer = Mock()
        events.subscribe(events.FINISHED_WORKUNIT, observer.on_file_processed)

        filename = self.model.get_current_filename()
        accepts_before_next_file = 9

        while accepts_before_next_file > 1:
            self.model.accept_current_item()
            self.model.next_item()
            assert_that(observer.on_file_processed.call_count, equal_to(0))
            accepts_before_next_file -= 1

        self.model.accept_current_item()
        assert_that(observer.on_file_processed.call_count, equal_to(1))

        # Make sure it was triggered with the right data
        args = observer.on_file_processed.call_args[0]
        assert_that(args, has_length(1))

        msg = args[0]
        assert_that(msg.topic, equal_to(events.FINISHED_WORKUNIT))
        assert_that(msg.data, equal_to(filename))

    def test_unlock_on_exit(self):
        current_file = self.model.get_current_filename()

        assert_that(self.progress_manager.owns_lock(current_file), equal_to(True))
        self.model.exit()
        assert_that(self.progress_manager.owns_lock(current_file), equal_to(False))


class RealsModelPersistenceLoadingTest(GeneralModelTest):
    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def setUp(self):
        # Have to set this up here because test cases may modify the .PART file
        partfile = os.path.join(self.get_directory_to_clean(), "xxx3.reals.astrom.PART")
        with open(partfile, "w+b") as filehandle:
            filehandle.write("0\n1\n")

        super(RealsModelPersistenceLoadingTest, self).setUp()

    def _get_task(self):
        return tasks.REALS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return RealsWorkUnitBuilder(parser, progress_manager)

    def _get_working_dir(self):
        return self.get_abs_path("data/model_persistence_partial")

    def get_directory_to_clean(self):
        return self._get_working_dir()

    def get_files_to_keep(self):
        return ["xxx1.cands.astrom", "xxx3.reals.astrom"]

    def test_load_partially_processed(self):
        observer = Mock()
        events.subscribe(events.FINISHED_WORKUNIT, observer)

        assert_that(self.model.get_current_workunit().get_unprocessed_sources(),
                    has_length(1))

        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(0))
        self.model.reject_current_item()

        self.model.next_item()
        self.model.accept_current_item()

        self.model.next_item()
        self.model.accept_current_item()

        assert_that(observer.call_count, equal_to(1))

        msg = observer.call_args_list[0][0][0]
        assert_that(msg.data, equal_to("xxx3.reals.astrom"))


class CandidatesModelPersistenceTest(GeneralModelTest):
    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def setUp(self):
        super(CandidatesModelPersistenceTest, self).setUp()

        concurrent_directory_manager = DirectoryContext(self._get_working_dir())
        self.concurrent_progress_manager = ProgressManager(concurrent_directory_manager)

    def _get_task(self):
        return tasks.CANDS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

    def _get_working_dir(self):
        return self.get_abs_path(FRESH_TEST_DIR)

    def get_directory_to_clean(self):
        return self._get_working_dir()

    def get_files_to_keep(self):
        return TEST_FILES

    def test_record_progress_cands_multiple_files(self):
        first_file = self.model.get_current_filename()

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(first_file),
                    equal_to(True))
        assert_that(self.concurrent_progress_manager.get_processed_indices(first_file),
                    contains_inanyorder(0, 1, 2))

        self.model.next_item()

        second_file = self.model.get_current_filename()
        assert_that(second_file, is_not(equal_to(first_file)))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    has_length(0))

        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(False))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0, 1))

        self.model.next_item()
        self.model.accept_current_item()
        assert_that(self.concurrent_progress_manager.is_done(second_file),
                    equal_to(True))
        assert_that(self.concurrent_progress_manager.get_processed_indices(second_file),
                    contains_inanyorder(0, 1, 2))


class CandidatesModelPersistenceLoadingTest(GeneralModelTest):
    def get_model(self):
        return models.UIModel(
            self.workunit_provider, self.progress_manager, self.download_manager)

    def create_part_file_with_indices(self, indices):
        str_indices = ""
        for index in indices:
            str_indices += str(index) + "\n"

        # Have to set this up here because test cases may modify the .PART file
        partfile = os.path.join(self.get_directory_to_clean(), "xxx1.cands.astrom.PART")
        with open(partfile, "w+b") as filehandle:
            filehandle.write(str_indices)

    def _get_task(self):
        return tasks.CANDS_TASK

    def _get_workunit_builder(self, parser, progress_manager):
        return CandidatesWorkUnitBuilder(parser, progress_manager)

    def _get_working_dir(self):
        return self.get_abs_path("data/model_persistence_partial")

    def get_directory_to_clean(self):
        return self._get_working_dir()

    def get_files_to_keep(self):
        return ["xxx1.cands.astrom", "xxx3.reals.astrom"]

    def test_load_partially_processed(self):
        self.create_part_file_with_indices([0, 2])
        super(CandidatesModelPersistenceLoadingTest, self).setUp()

        observer = Mock()
        events.subscribe(events.FINISHED_WORKUNIT, observer)

        assert_that(self.model.get_current_workunit().get_unprocessed_sources(),
                    has_length(1))

        assert_that(self.model.get_current_source_number(), equal_to(1))
        assert_that(self.model.get_current_obs_number(), equal_to(0))

        self.model.accept_current_item()
        assert_that(observer.call_count, equal_to(1))

        msg = observer.call_args_list[0][0][0]
        assert_that(msg.data, equal_to("xxx1.cands.astrom"))

    def test_load_fast_forward_through_sources(self):
        self.create_part_file_with_indices([0, 1])
        super(CandidatesModelPersistenceLoadingTest, self).setUp()

        assert_that(self.model.get_current_workunit().get_unprocessed_sources(),
                    has_length(1))

        assert_that(self.model.get_current_source_number(), equal_to(2))
        assert_that(self.model.get_current_obs_number(), equal_to(0))


if __name__ == '__main__':
    unittest.main()

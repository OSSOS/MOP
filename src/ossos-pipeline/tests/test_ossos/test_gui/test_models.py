__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock, patch
from hamcrest import assert_that, equal_to
from ossos.gui import events

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.gui.context import LocalDirectoryWorkingContext
from ossos.gui.models.imagemanager import ImageManager
from ossos.gui.models.validation import ValidationModel
from ossos.gui.models.transactions import TransAckValidationModel
from ossos.gui.progress import LocalProgressManager
from ossos.gui.sync import SynchronizationManager
from ossos.gui.models.workload import (PreFetchingWorkUnitProvider,
                                       RealsWorkUnit, CandidatesWorkUnit)


class ValidationModelTest(unittest.TestCase):
    def setUp(self):
        events.unsub_all()
        self.workunit_provider = Mock(spec=PreFetchingWorkUnitProvider)
        self.image_manager = Mock(spec=ImageManager)
        self.synchronization_manager = Mock(spec=SynchronizationManager)
        self.model = ValidationModel(self.workunit_provider, self.image_manager,
                                     self.synchronization_manager)

    def test_all_workunits_unlocked_on_exit(self):
        workunit1 = MagicMock(spec=RealsWorkUnit)
        workunit2 = MagicMock(spec=RealsWorkUnit)

        self.model.add_workunit(workunit1)
        self.model.add_workunit(workunit2)

        self.model.exit()

        workunit1.unlock.assert_called_once_with()
        workunit2.unlock.assert_called_once_with()

    def test_workunit_provider_shutdown_on_exit(self):
        self.model.exit()
        self.workunit_provider.shutdown.assert_called_once_with()


class TransitionAcknowledgementModelTest(FileReadingTestCase):
    def setUp(self):
        events.unsub_all()
        self.workunit_provider = Mock(spec=PreFetchingWorkUnitProvider)
        self.image_manager = Mock(spec=ImageManager)
        self.synchronization_manager = Mock(spec=SynchronizationManager)
        self.model = TransAckValidationModel(self.workunit_provider, self.image_manager,
                                             self.synchronization_manager)

        self.data = AstromParser().parse(
            self.get_abs_path("data/model_testdir_1/1584431p15.measure3.reals.astrom"))
        self.output_context = Mock(spec=LocalDirectoryWorkingContext)
        self.progress_manager = MagicMock(spec=LocalProgressManager)

        self.sources = self.data.get_sources()

    def use_reals_workunit(self):
        self.workunit = RealsWorkUnit("file", self.data, self.progress_manager,
                                      self.output_context)
        self.model.add_workunit(self.workunit)
        assert_that(self.model.get_current_reading(), equal_to(self.sources[0].get_reading(0)))

    def use_cands_workunit(self):
        self.workunit = CandidatesWorkUnit("file", self.data, self.progress_manager,
                                           self.output_context)
        self.workunit.get_writer = Mock()
        self.model.add_workunit(self.workunit)
        assert_that(self.model.get_current_reading(), equal_to(self.sources[0].get_reading(0)))

    def test_ignores_change_observations_while_transitioning_observations(self):
        self.use_reals_workunit()

        first_reading = self.sources[0].get_reading(0)
        second_reading = self.sources[0].get_reading(1)
        third_reading = self.sources[0].get_reading(2)

        assert_that(self.model.get_current_reading(), equal_to(first_reading))

        # This should cause the model to enter the waiting state.
        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(second_reading))

        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(second_reading))

        self.model.previous_obs()

        assert_that(self.model.get_current_reading(), equal_to(second_reading))

        self.model.acknowledge_image_displayed()

        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(third_reading))

    def test_ignores_change_sources_while_transitioning_sources(self):
        self.use_reals_workunit()

        first_source = self.sources[0]
        second_source = self.sources[1]
        third_source = self.sources[2]

        assert_that(self.model.get_current_source(), equal_to(first_source))

        # This should cause the model to enter the waiting state.
        self.model.next_source()

        assert_that(self.model.get_current_source(), equal_to(second_source))

        self.model.next_source()

        assert_that(self.model.get_current_source(), equal_to(second_source))

        self.model.previous_source()

        assert_that(self.model.get_current_source(), equal_to(second_source))

        self.model.acknowledge_image_displayed()

        self.model.next_source()

        assert_that(self.model.get_current_source(), equal_to(third_source))

    def test_ignores_next_item_while_transitioning_items(self):
        self.use_cands_workunit()

        first_source = self.sources[0]
        second_source = self.sources[1]
        third_source = self.sources[2]

        assert_that(self.model.get_current_source(), equal_to(first_source))

        # This should cause the model to enter the waiting state.
        self.model.next_item()

        assert_that(self.model.get_current_source(), equal_to(second_source))

        self.model.next_item()

        assert_that(self.model.get_current_source(), equal_to(second_source))

        self.model.acknowledge_image_displayed()

        self.model.next_item()

        assert_that(self.model.get_current_source(), equal_to(third_source))

    def test_reals_ignores_accept_while_transitioning_observations(self):
        self.use_reals_workunit()

        first_reading = self.sources[0].get_reading(0)
        second_reading = self.sources[0].get_reading(1)

        assert_that(self.model.get_current_reading(), equal_to(first_reading))

        # This should cause the model to enter the waiting state.
        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(second_reading))

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.accept_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(0))

        self.model.acknowledge_image_displayed()

        self.model.accept_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    def test_reals_ignores_reject_while_transitioning_observations(self):
        self.use_reals_workunit()

        first_reading = self.sources[0].get_reading(0)
        second_reading = self.sources[0].get_reading(1)

        assert_that(self.model.get_current_reading(), equal_to(first_reading))

        # This should cause the model to enter the waiting state.
        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(second_reading))

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.reject_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(0))

        self.model.acknowledge_image_displayed()

        self.model.reject_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    def test_cands_allow_accept_while_transitioning_observations(self):
        self.use_cands_workunit()

        self.model.next_obs()

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.accept_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    def test_cands_allow_reject_while_transitioning_observations(self):
        self.use_cands_workunit()

        self.model.next_obs()

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.reject_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    def test_cands_ignore_accept_while_transitioning_sources(self):
        self.use_cands_workunit()

        # This should cause the model to enter the waiting state.
        self.model.next_source()

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.accept_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(0))

        self.model.acknowledge_image_displayed()

        self.model.accept_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    def test_cands_ignore_reject_while_transitioning_sources(self):
        self.use_cands_workunit()

        # This should cause the model to enter the waiting state.
        self.model.next_source()

        assert_that(self.model.get_num_items_processed(), equal_to(0))
        self.model.reject_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(0))

        self.model.acknowledge_image_displayed()

        self.model.reject_current_item()
        assert_that(self.model.get_num_items_processed(), equal_to(1))

    @patch("ossos.gui.models.validation.events")
    def test_additional_change_image_events_not_sent_when_waiting(self, events_mock):
        self.use_reals_workunit()

        # This should cause the model to enter the waiting state.
        self.model.next_item()

        assert_that(events_mock.send.call_count, equal_to(1))

        self.model.next_item()

        assert_that(events_mock.send.call_count, equal_to(1))

        self.model.acknowledge_image_displayed()

        self.model.next_item()

        assert_that(events_mock.send.call_count, equal_to(2))


if __name__ == '__main__':
    unittest.main()

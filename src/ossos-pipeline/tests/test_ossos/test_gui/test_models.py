__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock
from hamcrest import assert_that, equal_to

from tests.base_tests import FileReadingTestCase
from ossos.astrom import AstromParser
from ossos.gui.context import LocalDirectoryWorkingContext
from ossos.gui.models import UIModel, TransitionAcknowledgementUIModel
from ossos.gui.downloads import AsynchronousImageDownloadManager
from ossos.gui.persistence import LocalProgressManager
from ossos.gui.sync import SynchronizationManager
from ossos.gui.workload import PreFetchingWorkUnitProvider, RealsWorkUnit


class UIModelTest(unittest.TestCase):
    def setUp(self):
        self.workunit_provider = Mock(spec=PreFetchingWorkUnitProvider)
        self.download_manager = Mock(spec=AsynchronousImageDownloadManager)
        self.synchronization_manager = Mock(spec=SynchronizationManager)
        self.model = UIModel(self.workunit_provider, self.download_manager,
                             self.synchronization_manager)

    def test_all_workunits_unlocked_on_exit(self):
        workunit1 = Mock(spec=RealsWorkUnit)
        workunit2 = Mock(spec=RealsWorkUnit)

        self.model.add_workunit(workunit1)
        self.model.add_workunit(workunit2)

        self.model.exit()

        workunit1.unlock.assert_called_once_with()
        workunit2.unlock.assert_called_once_with()

    def test_workunit_provider_shutdown_on_exit(self):
        self.model.exit()
        self.workunit_provider.shutdown.assert_called_once_with()


class TransitionAcknowledgementUIModelTest(FileReadingTestCase):
    def setUp(self):
        self.workunit_provider = Mock(spec=PreFetchingWorkUnitProvider)
        self.download_manager = Mock(spec=AsynchronousImageDownloadManager)
        self.synchronization_manager = Mock(spec=SynchronizationManager)
        self.model = TransitionAcknowledgementUIModel(self.workunit_provider, self.download_manager, self.synchronization_manager)

        self.data = AstromParser().parse(
            self.get_abs_path("data/model_testdir_1/1584431p15.measure3.reals.astrom"))
        self.output_context = Mock(spec=LocalDirectoryWorkingContext)
        self.progress_manager = MagicMock(spec=LocalProgressManager)
        self.workunit = RealsWorkUnit("file", self.data, self.progress_manager,
                                      self.output_context)

        self.model.add_workunit(self.workunit)

        self.sources = self.data.get_sources()
        assert_that(self.model.get_current_reading(), equal_to(self.sources[0].get_reading(0)))

    def test_ignores_changes_observations_while_expecting_acknowledgement(self):
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

        self.model.acknowledge_image_displayed(second_reading)

        self.model.next_obs()

        assert_that(self.model.get_current_reading(), equal_to(third_reading))

    def test_ignores_changes_sources_while_expecting_acknowledgement(self):
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

        self.model.acknowledge_image_displayed(second_source.get_reading(0))

        self.model.next_source()

        assert_that(self.model.get_current_source(), equal_to(third_source))


if __name__ == '__main__':
    unittest.main()

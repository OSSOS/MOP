__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock

from ossos.gui.models import UIModel
from ossos.gui.downloads import AsynchronousImageDownloadManager
from ossos.gui.persistence import LocalProgressManager
from ossos.gui.sync import SynchronizationManager
from ossos.gui.workload import PreFetchingWorkUnitProvider, RealsWorkUnit


class UIModelTest(unittest.TestCase):
    def setUp(self):
        self.workunit_provider = Mock(spec=PreFetchingWorkUnitProvider)
        self.progress_manager = Mock(spec=LocalProgressManager)
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


if __name__ == '__main__':
    unittest.main()

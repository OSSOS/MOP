__author__ = "David Rusk <drusk@uvic.ca>"

import copy
import unittest

from hamcrest import assert_that, is_in, is_not, equal_to
from mock import Mock

from pymop.io.workload import (WorkUnitFactory, DirectoryManager,
                               NoAvailableWorkException)
from pymop.io.astrom import AstromParser


class TestProgressManager(object):
    """
    A partial test implementation of the ProgressManager interface which
    just remembers values set on it without persisting them to disk in
    any way.
    """

    def __init__(self):
        self.done = []

    def is_done(self, filename):
        return filename in self.done

    def record_done(self, filename):
        self.done.append(filename)


class WorkUnitFactoryTest(unittest.TestCase):
    def test_create_workload_fresh_directory(self):
        file1 = "file1"
        file2 = "file2"
        test_files = [file1, file2]

        def _get_listing():
            return copy.deepcopy(test_files)

        directory_manager = Mock(spec=DirectoryManager)
        directory_manager.get_listing = _get_listing

        progress_manager = TestProgressManager()
        parser = Mock(spec=AstromParser)

        undertest = WorkUnitFactory(directory_manager, progress_manager, parser)

        workunit1 = undertest.create_workunit()
        assert_that(workunit1.get_filename(), is_in(test_files))
        progress_manager.record_done(workunit1.get_filename())

        workunit2 = undertest.create_workunit()
        assert_that(workunit2.get_filename(), is_in(test_files))
        assert_that(workunit2.get_filename(),
                    is_not(equal_to(workunit1.get_filename())))
        progress_manager.record_done(workunit2.get_filename())

        self.assertRaises(NoAvailableWorkException, undertest.create_workunit)


if __name__ == '__main__':
    unittest.main()

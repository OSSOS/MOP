__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import (assert_that, is_in, is_not, equal_to, is_, none,
                      contains_inanyorder, has_length, contains)
from mock import Mock, call, MagicMock

from tests.base_tests import FileReadingTestCase, DirectoryCleaningTestCase
from tests.testutil import CopyingMock
from ossos.gui import tasks
from ossos.gui.context import WorkingContext, LocalDirectoryWorkingContext
from ossos.gui.models.collections import StatefulCollection
from ossos.gui.models.exceptions import NoAvailableWorkException
from ossos.gui.models.validation import ValidationModel
from ossos.astrom import (AstromParser, StreamingAstromWriter, SourceReading,
                          Source)
from ossos.mpc import MPCWriter
from ossos.gui.models.imagemanager import ImageManager
from ossos.gui.progress import LocalProgressManager, InMemoryProgressManager
from ossos.gui.models.workload import (WorkUnitProvider, WorkUnit,
                                       RealsWorkUnit, CandidatesWorkUnit,
                                       RealsWorkUnitBuilder,
                                       PreFetchingWorkUnitProvider, TracksWorkUnitBuilder)
from ossos.ssos import SSOSData, SSOSParser


class TestDirectoryManager(object):
    def __init__(self):
        self.listings = {}

    def set_listing(self, suffix, listing):
        self.listings[suffix] = listing[:]

    def get_listing(self, suffix):
        return self.listings[suffix][:]

    def get_full_path(self, filename):
        return filename

    def get_file_size(self, filename):
        return 1


class TestWorkUnitBuilder(object):
    def build_workunit(self, full_path):
        _, filename = os.path.split(full_path)
        workunit = Mock(spec=WorkUnit)
        workunit.get_filename.return_value = filename
        return workunit


class AbstractWorkUnitTest(FileReadingTestCase):
    def setUp(self):
        self.testfile = self.get_input_file()
        parser = AstromParser()
        self.data = parser.parse(self.get_abs_path(self.testfile))
        self.progress_manager = Mock(spec=LocalProgressManager)
        self.progress_manager.get_processed_indices.return_value = []
        self.output_context = Mock(spec=WorkingContext)

    def get_input_file(self):
        raise NotImplementedError()


class WorkUnitTest(AbstractWorkUnitTest):
    def get_input_file(self):
        return "data/1584431p15.measure3.reals.astrom"

    def setUp(self):
        super(WorkUnitTest, self).setUp()

        self.workunit = RealsWorkUnit(self.testfile, self.data,
                                      self.progress_manager,
                                      self.output_context)
        self.writer = Mock(spec=MPCWriter)
        self.workunit.get_writer = Mock(return_value=self.writer)

    def test_initialization(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_source_count(), equal_to(3))
        assert_that(self.workunit.get_obs_count(), equal_to(3))

    def test_next_source_previous_source(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_next_source_wrap(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(2))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_next_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_previous_source_wrap(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(2))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        self.workunit.previous_source()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))

    def test_previous_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.previous_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_next_source_resets_obs(self):
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))
        self.workunit.next_source()
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))
        self.workunit.next_obs()
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))


class RealsWorkUnitTest(AbstractWorkUnitTest):
    def get_input_file(self):
        return "data/1584431p15.measure3.reals.astrom"

    def setUp(self):
        super(RealsWorkUnitTest, self).setUp()

        self.workunit = RealsWorkUnit(self.testfile, self.data,
                                      self.progress_manager,
                                      self.output_context)
        self.writer = Mock(spec=MPCWriter)
        self.workunit.get_writer = Mock(return_value=self.writer)

    def test_next_vettable_item_no_validation(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))

        self.workunit.next_item()
        # Should have looped back to first observation of the same source
        # because we haven't finished processing it.
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_next_vettable_item_after_validate_last(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))

        self.workunit.accept_current_item()
        self.workunit.next_item()

        # Should have looped back to first observation of the same source
        # because we haven't finished processing it.
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.accept_current_item()
        self.workunit.next_item()

        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))

        self.workunit.accept_current_item()
        self.workunit.next_item()

        # We already validated the last reading, so we should be jumping
        # straight to the second source now.

        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()

        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))

    def test_next_vettable_item_jump_over_processed(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(1))

        self.workunit.reject_current_item()
        self.workunit.next_item()

        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(2))

    def test_accept_current_item(self):
        first_source = self.data.get_sources()[0]
        first_item = first_source.get_reading(0)
        second_item = first_source.get_reading(1)

        assert_that(self.workunit.is_item_processed(first_item), equal_to(False))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.accept_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(True))

    def test_reject_current_item(self):
        first_source = self.data.get_sources()[0]
        first_item = first_source.get_reading(0)
        second_item = first_source.get_reading(1)

        assert_that(self.workunit.is_item_processed(first_item), equal_to(False))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.reject_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.next_item()
        self.workunit.reject_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(True))

    def test_get_current_item_index(self):
        for index in range(8):
            assert_that(self.workunit.get_current_item_index(), equal_to(index))
            self.workunit.accept_current_item()
            self.workunit.next_item()

        assert_that(self.workunit.get_current_item_index(), equal_to(8))

    def test_get_unprocessed_sources(self):
        sources = self.data.get_sources()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(*sources))

        self.workunit.accept_current_item()
        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(*sources))

        self.workunit.next_item()
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(sources[1], sources[2]))

        self.workunit.next_item()
        self.workunit.reject_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(sources[2]))

        self.workunit.next_item()
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    has_length(0))

    def test_finish_workunit_unlocks_file(self):
        num_items = 9
        while num_items > 1:
            self.workunit.accept_current_item()
            self.workunit.next_item()
            num_items -= 1

        assert_that(self.progress_manager.unlock.called, equal_to(False))

        self.workunit.accept_current_item()

        self.progress_manager.unlock.assert_called_once_with(
            self.testfile, do_async=True)

    def test_flush_only_after_source_finished(self):
        assert_that(self.writer.flush.called, equal_to(False))

        self.workunit.accept_current_item()
        self.workunit.next_item()

        assert_that(self.writer.flush.called, equal_to(False))

        self.workunit.reject_current_item()
        self.workunit.next_item()

        assert_that(self.writer.flush.called, equal_to(False))

        self.workunit.accept_current_item()
        assert_that(self.writer.flush.call_count, equal_to(1))

    def test_get_output_filename_dry_run(self):
        self.workunit = RealsWorkUnit(self.testfile, self.data,
                                      self.progress_manager,
                                      self.output_context,
                                      dry_run=True)
        source = self.data.get_sources()[0]
        provisional_name = "ABCD123"
        source.set_provisional_name(provisional_name)

        assert_that(self.workunit.get_output_filename(source),
                    equal_to("1584431p15.measure3.reals.astrom.ABCD123.mpc"))

    def test_get_output_filename_not_dry_run(self):
        self.workunit = RealsWorkUnit(self.testfile, self.data,
                                      self.progress_manager,
                                      self.output_context,
                                      dry_run=False)
        source = self.data.get_sources()[0]
        provisional_name = "ABCD123"
        source.set_provisional_name(provisional_name)

        assert_that(self.workunit.get_output_filename(source),
                    equal_to("1584431p15.measure3.reals.astrom.ABCD123.mpc"))


class CandidatesWorkUnitTest(AbstractWorkUnitTest):
    def get_input_file(self):
        return "data/1584431p15.measure3.cands.astrom"

    def setUp(self):
        super(CandidatesWorkUnitTest, self).setUp()

        self.workunit = CandidatesWorkUnit(self.testfile, self.data,
                                           self.progress_manager,
                                           self.output_context)
        self.writer = Mock(spec=StreamingAstromWriter)
        self.workunit.get_writer = Mock(return_value=self.writer)

    def test_output_filename(self):
        assert_that(self.workunit.get_output_filename(),
                    equal_to(self.testfile.replace(".cands", ".reals")))

    def test_next_vettable_item(self):
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(1))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(2))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

        self.workunit.next_item()
        assert_that(self.workunit.get_current_source_number(), equal_to(0))
        assert_that(self.workunit.get_current_obs_number(), equal_to(0))

    def test_accept_current_item(self):
        first_item = self.data.get_sources()[0]
        second_item = self.data.get_sources()[1]

        assert_that(self.workunit.is_item_processed(first_item), equal_to(False))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.accept_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(True))

    def test_reject_current_item(self):
        first_item = self.data.get_sources()[0]
        second_item = self.data.get_sources()[1]

        assert_that(self.workunit.is_item_processed(first_item), equal_to(False))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.reject_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(False))

        self.workunit.next_item()
        self.workunit.reject_current_item()

        assert_that(self.workunit.is_item_processed(first_item), equal_to(True))
        assert_that(self.workunit.is_item_processed(second_item), equal_to(True))

    def test_get_unprocessed_sources(self):
        sources = self.data.get_sources()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(*sources))

        self.workunit.accept_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(sources[1], sources[2]))

        self.workunit.next_item()
        self.workunit.reject_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    contains_inanyorder(sources[2]))

        self.workunit.next_item()
        self.workunit.accept_current_item()

        assert_that(self.workunit.get_unprocessed_sources(),
                    has_length(0))

    def test_finish_workunit_unlocks_file(self):
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()
        self.workunit.next_item()
        self.workunit.accept_current_item()

        self.progress_manager.unlock.assert_called_once_with(
            self.testfile, do_async=True)


class StatefulCollectionTest(unittest.TestCase):
    def test_basics(self):
        items = [1, 2, 3]
        undertest = StatefulCollection(items)

        assert_that(undertest[0], equal_to(items[0]))
        assert_that(undertest[1], equal_to(items[1]))
        assert_that(undertest[2], equal_to(items[2]))

        assert_that(undertest, has_length(3))

        assert_that(undertest.get_index(), equal_to(0))
        assert_that(undertest.get_current_item(), equal_to(items[0]))

        next(undertest)
        assert_that(undertest.get_index(), equal_to(1))
        assert_that(undertest.get_current_item(), equal_to(items[1]))

        undertest.previous()
        assert_that(undertest.get_index(), equal_to(0))
        assert_that(undertest.get_current_item(), equal_to(items[0]))

    def test_start_empty(self):
        undertest = StatefulCollection()

        assert_that(undertest, has_length(0))

        assert_that(undertest.get_current_item(), is_(none()))
        assert_that(undertest.get_index(), equal_to(-1))

        item1 = 1

        undertest.append(item1)
        assert_that(undertest.get_current_item(), equal_to(item1))
        assert_that(undertest.get_index(), equal_to(0))

        item2 = 2
        undertest.append(item2)

        next(undertest)
        assert_that(undertest.get_index(), equal_to(1))
        assert_that(undertest.get_current_item(), equal_to(item2))


class WorkloadManagementTest(unittest.TestCase):
    def setUp(self):
        self.progress_manager = InMemoryProgressManager(Mock(spec=LocalDirectoryWorkingContext))
        self.workunit_provider = Mock(spec=WorkUnitProvider)

        self.workunit1 = MagicMock(spec=WorkUnit)
        self.file1 = "file1"
        self.workunit1.get_filename.return_value = self.file1

        self.workunit2 = MagicMock(spec=WorkUnit)
        self.file2 = "file2"
        self.workunit2.get_filename.return_value = self.file2

        workunits = [self.workunit1, self.workunit2]

        def get_workunit(index):
            workunit = workunits[index]
            self.progress_manager.lock(workunit.get_filename())
            return workunit

        self.workunit_provider.get_workunit.side_effect = (get_workunit(index) for index in range(2))
        image_manager = Mock(spec=ImageManager)

        self.undertest = ValidationModel(self.workunit_provider, image_manager, None)
        self.undertest.start_work()

    def test_workunits_on_demand(self):
        assert_that(self.undertest.get_current_workunit(), equal_to(self.workunit1))
        assert_that(self.workunit_provider.get_workunit.call_count, equal_to(1))

        self.undertest.next_workunit()
        assert_that(self.undertest.get_current_workunit(), equal_to(self.workunit2))
        assert_that(self.workunit_provider.get_workunit.call_count, equal_to(2))

    def test_shift_locks(self):
        assert_that(self.undertest.get_current_workunit(), equal_to(self.workunit1))
        assert_that(self.progress_manager.owns_lock(self.file1), equal_to(True))
        assert_that(self.progress_manager.owns_lock(self.file2), equal_to(False))

        self.undertest.next_workunit()
        assert_that(self.undertest.get_current_workunit(), equal_to(self.workunit2))

        # Note we don't give up lock just by going to next workunit.  It is
        # released once it is done processing.
        assert_that(self.progress_manager.owns_lock(self.file1), equal_to(True))
        assert_that(self.progress_manager.owns_lock(self.file2), equal_to(True))


class WorkUnitProviderTest(unittest.TestCase):
    def setUp(self):
        self.taskid = "id"
        self.file1 = "file1"
        self.file2 = "file2"
        self.file3 = "file3"
        self.file4 = "file4"
        self.test_files = [self.file1, self.file2]

        directory_manager = TestDirectoryManager()
        progress_manager = InMemoryProgressManager(directory_manager)
        builder = TestWorkUnitBuilder()

        self.undertest = WorkUnitProvider(self.taskid, directory_manager,
                                          progress_manager, builder)
        self.directory_manager = directory_manager
        self.progress_manager = progress_manager
        self.directory_manager.set_listing(self.taskid, self.test_files)

    def test_create_workload_acquires_lock(self):
        self.directory_manager.set_listing(self.taskid, self.test_files)
        workunit1 = self.undertest.get_workunit()
        assert_that(self.progress_manager.owns_lock(workunit1.get_filename()),
                    equal_to(True))

    def test_create_workload_fresh_directory(self):
        workunit1 = self.undertest.get_workunit()
        assert_that(workunit1.get_filename(), is_in(self.test_files))
        self.progress_manager.record_done(workunit1.get_filename())

        workunit2 = self.undertest.get_workunit()
        assert_that(workunit2.get_filename(), is_in(self.test_files))
        assert_that(workunit2.get_filename(),
                    is_not(equal_to(workunit1.get_filename())))
        self.progress_manager.record_done(workunit2.get_filename())

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_create_workload_one_file_already_done(self):
        self.progress_manager.done.add(self.file1)

        workunit = self.undertest.get_workunit()
        assert_that(workunit.get_filename(), equal_to(self.file2))
        self.progress_manager.record_done(self.file2)

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_create_workload_locked_files(self):
        self.progress_manager.add_external_lock(self.file2)

        workunit = self.undertest.get_workunit()
        assert_that(workunit.get_filename(), equal_to(self.file1))
        self.progress_manager.record_done(self.file1)

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_items_in_ignore_list_ignored(self):
        workunit = self.undertest.get_workunit(ignore_list=[self.file1])

        assert_that(workunit.get_filename(), equal_to(self.file2))
        self.progress_manager.record_done(self.file2)

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit,
                          ignore_list=[self.file1])

    def test_file_found_to_be_done_not_checked_again(self):
        test_files = [self.file1, self.file2, self.file3, self.file4]
        self.directory_manager.set_listing(self.taskid, test_files)

        self.progress_manager.lock(self.file2)
        self.progress_manager.record_done(self.file2)
        self.progress_manager.unlock(self.file2)

        # We don't yet know file1 is done.
        assert_that(self.undertest.get_potential_files([]),
                    contains_inanyorder(self.file1, self.file2, self.file3,
                                        self.file4))
        assert_that(self.undertest.get_workunit().get_filename(), equal_to(self.file1))

        # We have not yet discovered file2 is done because we found file 1
        # right away.  However, we should remember we already returned file1.
        assert_that(self.undertest.get_potential_files([]),
                    contains_inanyorder(self.file2, self.file3, self.file4))

        # Here we should discover file2 is done and skip over it
        assert_that(self.undertest.get_workunit().get_filename(), equal_to(self.file3))

        # So the next time we know not to check file2 again
        assert_that(self.undertest.get_potential_files([]),
                    contains_inanyorder(self.file4))


class PreFetchingWorkUnitProviderTest(unittest.TestCase):
    def setUp(self):
        self.prefetch_quantity = 2
        self.workunit_provider = CopyingMock(spec=WorkUnitProvider)
        self.undertest = PreFetchingWorkUnitProvider(self.workunit_provider,
                                                     self.prefetch_quantity)
        self._workunit_number = 0

    def create_workunit(self, num=None):
        if num is None:
            num = self._workunit_number

        workunit = Mock(spec=WorkUnit)
        workunit.get_filename.return_value = "Workunit%d" % num
        self._workunit_number += 1
        return workunit

    def set_workunit_provider_return_values(self, return_values):
        def side_effect(*args, **kwargs):
            result = return_values.pop(0)
            if isinstance(result, Exception):
                raise result
            return result

        self.workunit_provider.get_workunit.side_effect = side_effect

    def mock_prefetch_workunit(self, bypass_threading=False):
        prefetch_workunit_mock = Mock()

        if bypass_threading:
            def do_prefetch():
                self.undertest._do_prefetch_workunit()

            prefetch_workunit_mock.side_effect = do_prefetch

        else:
            def generate_mocked_workunit():
                self.undertest.workunits.append(self.create_workunit())

            prefetch_workunit_mock.side_effect = generate_mocked_workunit

        self.undertest.prefetch_workunit = prefetch_workunit_mock

        return prefetch_workunit_mock

    def test_get_first_workunit_also_prefetches_configured_quantity(self):
        prefetch_workunit_mock = self.mock_prefetch_workunit()

        self.undertest.get_workunit()
        assert_that(prefetch_workunit_mock.call_count,
                    equal_to(self.prefetch_quantity))

    def test_get_second_workunit_prefetches_one_more(self):
        prefetch_workunit_mock = self.mock_prefetch_workunit()

        self.undertest.get_workunit()
        self.undertest.get_workunit()
        assert_that(prefetch_workunit_mock.call_count,
                    equal_to(self.prefetch_quantity + 1))

    def test_all_prefetched_doesnt_raise_no_available_work_until_all_retrieved(self):
        prefetch_workunit_mock = self.mock_prefetch_workunit(bypass_threading=True)

        workunit1 = self.create_workunit()
        workunit2 = self.create_workunit()
        workunit3 = self.create_workunit()

        self.set_workunit_provider_return_values(
            [workunit1, workunit2, workunit3, NoAvailableWorkException()])

        assert_that(self.undertest.get_workunit(), equal_to(workunit1))

        assert_that(prefetch_workunit_mock.call_count,
                    equal_to(self.prefetch_quantity))

        assert_that(self.undertest.get_workunit(), equal_to(workunit2))

        # This call to fetch will raise a NoAvailableWorkException
        assert_that(prefetch_workunit_mock.call_count,
                    equal_to(self.prefetch_quantity + 1))

        assert_that(self.undertest.get_workunit(), equal_to(workunit3))

        # Note we don't try to fetch anymore
        assert_that(prefetch_workunit_mock.call_count,
                    equal_to(self.prefetch_quantity + 1))

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_prefetch_excludes_already_fetched(self):
        prefetch_workunit_mock = self.mock_prefetch_workunit(bypass_threading=True)

        workunit1 = self.create_workunit()
        workunit2 = self.create_workunit()
        workunit3 = self.create_workunit()

        self.set_workunit_provider_return_values(
            [workunit1, workunit2, workunit3, NoAvailableWorkException()])

        self.undertest.get_workunit()
        self.undertest.get_workunit()
        self.undertest.get_workunit()
        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

        expected_calls = [
            call(ignore_list=[]),
            call(ignore_list=[workunit1.get_filename()]),
            call(ignore_list=[workunit1.get_filename(), workunit2.get_filename()]),
            call(ignore_list=[workunit1.get_filename(), workunit2.get_filename(),
                              workunit3.get_filename()])
        ]

        self.workunit_provider.get_workunit.assert_has_calls(expected_calls)

    def test_get_workunit_no_prefetch(self):
        self.prefetch_quantity = 0
        self.workunit_provider = Mock(spec=WorkUnitProvider)
        self.undertest = PreFetchingWorkUnitProvider(self.workunit_provider,
                                                     self.prefetch_quantity)
        prefetch_workunit_mock = self.mock_prefetch_workunit(bypass_threading=True)

        workunit1 = self.create_workunit()
        workunit2 = self.create_workunit()
        workunit3 = self.create_workunit()

        self.set_workunit_provider_return_values(
            [workunit1, workunit2, workunit3, NoAvailableWorkException()])

        assert_that(self.undertest.get_workunit(), equal_to(workunit1))

        assert_that(prefetch_workunit_mock.call_count, equal_to(0))

        assert_that(self.undertest.get_workunit(), equal_to(workunit2))

        assert_that(prefetch_workunit_mock.call_count, equal_to(0))

        assert_that(self.undertest.get_workunit(), equal_to(workunit3))

        assert_that(prefetch_workunit_mock.call_count, equal_to(0))

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

    def test_duplicate_workunit_ignored_(self):
        # This tests the case when 2 or more threads created back to back
        # end up retrieving the same workunit.  Only want to return it once
        # though.
        prefetch_workunit_mock = self.mock_prefetch_workunit(bypass_threading=True)

        workunit1 = self.create_workunit(1)
        workunit2 = self.create_workunit(2)
        workunit3 = self.create_workunit(2)

        self.set_workunit_provider_return_values(
            [workunit1, workunit2, workunit3, NoAvailableWorkException()])

        assert_that(self.undertest.get_workunit(), equal_to(workunit1))

        assert_that(self.undertest.get_workunit(), equal_to(workunit2))

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)


class WorkUnitProviderRealFilesTest(FileReadingTestCase, DirectoryCleaningTestCase):
    def setUp(self):
        working_directory = self.get_directory_to_clean()
        context = LocalDirectoryWorkingContext(working_directory)
        progress_manager = InMemoryProgressManager(context)
        parser = AstromParser()
        builder = RealsWorkUnitBuilder(parser, context, context, progress_manager)
        undertest = WorkUnitProvider(tasks.get_suffix(tasks.REALS_TASK),
                                     context,
                                     progress_manager,
                                     builder)
        self.progress_manager = progress_manager
        self.undertest = undertest

    def get_directory_to_clean(self):
        return self.get_abs_path("data/workload_testdir2")

    def get_files_to_keep(self):
        return ["candstest1.measure3.cands.astrom", "candstest2.measure3.cands.astrom",
                "candstest1.measure3.reals.astrom", "candstest2.measure3.reals.astrom",
                "realstest1.measure3.reals.astrom", "realstest2.measure3.reals.astrom"]

    def test_skip_empty_files(self):
        expected_filenames = ["realstest1.measure3.reals.astrom",
                              "realstest2.measure3.reals.astrom"]

        actual_filenames = []
        workunit1 = self.undertest.get_workunit()
        actual_filenames.append(workunit1.get_filename())
        self.progress_manager.record_done(workunit1.get_filename())

        workunit2 = self.undertest.get_workunit()
        actual_filenames.append(workunit2.get_filename())
        self.progress_manager.record_done(workunit2.get_filename())

        self.assertRaises(NoAvailableWorkException, self.undertest.get_workunit)

        assert_that(actual_filenames, contains_inanyorder(*expected_filenames))


class TracksWorkUnitBuilderTest(unittest.TestCase):
    def test_move_discovery_to_front(self):
        def mock_reading(discovery=False):
            reading = Mock(spec=SourceReading)
            reading.discovery = discovery
            return reading

        reading0 = mock_reading()
        reading1 = mock_reading(discovery=True)
        reading2 = mock_reading()
        reading3 = mock_reading()
        reading4 = mock_reading()

        data = SSOSData(Mock(),
                        [[reading0, reading1, reading2, reading3, reading4]],
                        "123456")

        builder = TracksWorkUnitBuilder(Mock(spec=SSOSParser),
                                        Mock(spec=LocalDirectoryWorkingContext),
                                        Mock(spec=LocalDirectoryWorkingContext),
                                        Mock(spec=LocalProgressManager))

        def get_readings(data):
            # Note: Track workunits only have 1 source
            return data.get_sources()[0].get_readings()

        assert_that(get_readings(data),
                    contains(reading0, reading1, reading2, reading3, reading4))

        builder.move_discovery_to_front(data)

        assert_that(get_readings(data),
                    contains(reading1, reading2, reading3, reading0, reading4))


if __name__ == '__main__':
    unittest.main()

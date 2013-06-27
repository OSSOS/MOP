__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop.io.persistence import FileLockedException
from pymop.io.astrom import Source


class NoAvailableWorkException(Exception):
    """"No more work is available."""


class WorkCompleteException(Exception):
    """Everything in the WorkUnit has been processed."""


class VettableItem(object):
    ACCEPTED = "accepted"
    REJECTED = "rejected"
    UNPROCESSED = "unprocessed"

    def __init__(self, item):
        self._status = VettableItem.UNPROCESSED
        self.item = item

    def __getattr__(self, attr):
        return getattr(self.item, attr)

    def is_processed(self):
        return self._status != VettableItem.UNPROCESSED

    def is_accepted(self):
        return self._status == VettableItem.ACCEPTED

    def is_rejected(self):
        return self._status == VettableItem.REJECTED

    def accept(self):
        self._status = VettableItem.ACCEPTED

    def reject(self):
        self._status = VettableItem.REJECTED

    def get_status(self):
        return self._status


class StatefulCollection(object):
    def __init__(self, items=None):
        if items is None:
            self.items = []
            self.index = -1
        else:
            self.items = items
            self.index = 0

        self.callbacks = []

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def __getitem__(self, index):
        return self.items[index]

    def __getattr__(self, attr):
        return getattr(self.items, attr)

    def add_callback(self, callback):
        self.callbacks.append(callback)

    def append(self, item):
        if len(self) == 0:
            # Special case, we make this the current item
            self.index = 0

        self.items.append(item)

    def get_index(self):
        return self.index

    def get_current_item(self):
        if self.items:
            return self.items[self.index]
        else:
            return None

    def next(self):
        self._move(1)

    def previous(self):
        self._move(-1)

    def has_next(self):
        return self.index < len(self) - 1

    def _move(self, delta):
        first_item = self.get_current_item()
        self.index = (self.index + delta) % len(self)
        second_item = self.get_current_item()

        for callback in self.callbacks:
            callback(first_item, second_item)


class WorkUnit(object):
    def __init__(self, filename, data_collection, results_writer):
        self.filename = filename
        self.data_collection = data_collection
        self.results_writer = results_writer

    def get_filename(self):
        return self.filename

    def get_data(self):
        return self.data_collection

    def get_sources(self):
        return self.data_collection.get_sources()

    def get_source_count(self):
        return len(self.get_sources())

    def get_current_source(self):
        return self.get_sources().get_current_item()

    def get_current_source_number(self):
        return self.get_sources().get_index()

    def get_current_source_readings(self):
        return self.get_current_source().get_readings()

    def get_obs_count(self):
        return len(self.get_current_source_readings())

    def get_current_reading(self):
        return self.get_current_source_readings().get_current_item()

    def get_current_obs_number(self):
        return self.get_current_source_readings().get_index()

    def next_source(self):
        self.get_sources().next()

    def previous_source(self):
        self.get_sources().previous()

    def next_obs(self):
        self.get_current_source_readings().next()

    def previous_obs(self):
        self.get_current_source_readings().previous()

    def accept_current_item(self):
        raise NotImplementedError()

    def reject_current_item(self):
        raise NotImplementedError()

    def next_vettable_item(self):
        raise NotImplementedError()

    def get_writer(self):
        return self.results_writer

    def is_finished(self):
        raise NotImplementedError()


class RealsWorkUnit(WorkUnit):
    def __init__(self, filename, data_collection, results_writer):
        super(RealsWorkUnit, self).__init__(
            filename, data_collection, results_writer)

    def accept_current_item(self):
        self.get_current_reading().accept()

    def reject_current_item(self):
        self.get_current_reading().reject()

    def next_vettable_item(self):
        num_obs_to_check = len(self.get_current_source_readings())
        while num_obs_to_check > 0:
            self.next_obs()
            if not self.get_current_reading().is_processed():
                # This observation needs to be vetted, stop here
                return

            num_obs_to_check -= 1

        # All observations of this source have been processed
        self.next_source()

    def get_current_item_index(self):
        return (self.get_sources().get_index() +
                self.get_current_source_readings().get_index())

    def is_finished(self):
        for source in self.get_sources():
            for reading in source.get_readings():
                if not reading.is_processed():
                    return False

        return True


class CandidatesWorkUnit(WorkUnit):
    def __init__(self, filename, data_collection, results_writer):
        super(CandidatesWorkUnit, self).__init__(
            filename, data_collection, results_writer)

    def accept_current_item(self):
        self.get_current_source().accept()

    def reject_current_item(self):
        self.get_current_source().reject()

    def next_vettable_item(self):
        self.next_source()

    def get_current_item_index(self):
        return self.get_sources().get_index()

    def is_finished(self):
        for source in self.get_sources():
            if not source.is_processed():
                return False

        return True


class WorkUnitProvider(object):
    def __init__(self,
                 taskid,
                 directory_manager,
                 progress_manager,
                 builder):
        self.taskid = taskid
        self.directory_manager = directory_manager
        self.progress_manager = progress_manager
        self.builder = builder

    def get_workunit(self):
        potential_files = self.directory_manager.get_listing(self.taskid)

        while len(potential_files) > 0:
            potential_file = potential_files.pop()

            if not self.progress_manager.is_done(potential_file):
                try:
                    self.progress_manager.lock(potential_file)
                except FileLockedException:
                    continue

                return self.builder.build_workunit(
                    self.directory_manager.get_full_path(potential_file))

        raise NoAvailableWorkException()


class DataCollection(object):
    def __init__(self, parsed_data):
        self.observations = parsed_data.observations
        self.sys_header = parsed_data.sys_header

        sources = []
        for source in parsed_data.get_sources():
            reading_collection = StatefulCollection(
                map(VettableItem, source.get_readings()))
            sources.append(VettableItem(Source(reading_collection)))

        self.source_collection = StatefulCollection(sources)

    def get_sources(self):
        return self.source_collection


class WorkUnitBuilder(object):
    def __init__(self, parser, writer_factory):
        self.parser = parser
        self.writer_factory = writer_factory

    def build_workunit(self, full_path):
        parsed_data = self.parser.parse(full_path)
        data_collection = DataCollection(parsed_data)

        _, filename = os.path.split(full_path)
        return self._build_workunit(filename, data_collection,
                                    self.writer_factory.create_writer(
                                        full_path, parsed_data))

    def _build_workunit(self, filename, data, writer):
        raise NotImplementedError()


class RealsWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, writer_factory):
        super(RealsWorkUnitBuilder, self).__init__(parser, writer_factory)

    def _build_workunit(self, filename, data, writer):
        return RealsWorkUnit(filename, data, writer)


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, writer_factory):
        super(CandidatesWorkUnitBuilder, self).__init__(parser, writer_factory)

    def _build_workunit(self, filename, data, writer):
        return CandidatesWorkUnit(filename, data, writer)


class WorkloadManager(object):
    """
    Manages the workload's state.
    """

    def __init__(self, workunit_provider, progress_manager):
        self.workunit_provider = workunit_provider
        self.progress_manager = progress_manager
        self.work_units = StatefulCollection()

        def shift_locks(workunit1, workunit2):
            self._unlock(workunit1)
            self._lock(workunit2)

        self.work_units.add_callback(shift_locks)

        self.num_processed = 0

    def _lock(self, workunit):
        self.progress_manager.lock(workunit.get_filename())

    def _unlock(self, workunit):
        self.progress_manager.unlock(workunit.get_filename())

    def __getattr__(self, attr):
        return getattr(self.get_current_workunit(), attr)

    def start_work(self):
        self.next_workunit()

    def next_workunit(self):
        if not self.work_units.has_next():
            self.work_units.append(self.workunit_provider.get_workunit())

        self.work_units.next()

    def previous_workunit(self):
        self.work_units.previous()

    def get_current_workunit(self):
        return self.work_units.get_current_item()

    def next_item(self):
        self.get_current_workunit().next_vettable_item()

    def previous_item(self):
        self.get_current_workunit().previous_vettable_item()

    def next_source(self):
        self.get_current_workunit().next_source()

    def next_obs(self):
        self.get_current_workunit().next_obs()

    def previous_source(self):
        self.get_current_workunit().previous_source()

    def previous_obs(self):
        self.get_current_workunit().previous_obs()

    def get_current_data(self):
        return self.get_current_workunit().get_data()

    def get_current_filename(self):
        return self.get_current_workunit().get_filename()

    def get_current_source(self):
        return self.get_current_workunit().get_current_source()

    def get_current_source_number(self):
        return self.get_current_workunit().get_current_source_number()

    def get_current_obs_number(self):
        return self.get_current_workunit().get_current_obs_number()

    def get_obs_count(self):
        return self.get_current_workunit().get_obs_count()

    def accept_current_item(self):
        self.get_current_workunit().accept_current_item()
        self._process_current_item()

    def reject_current_item(self):
        self.get_current_workunit().reject_current_item()
        self._process_current_item()

    def _process_current_item(self):
        self.num_processed += 1
        self.progress_manager.record_index(
            self.get_current_filename(),
            self.get_current_workunit().get_current_item_index())

        if self.get_current_workunit().is_finished():
            self.progress_manager.record_done(
                self.get_current_workunit().get_filename())
            self.next_workunit()

    def get_num_items_processed(self):
        return self.num_processed

    def get_writer(self):
        return self.get_current_workunit().get_writer()

    def count_unclaimed_readings(self):
        # TODO: a light-weight estimator
        return -1

    def exit(self):
        self._unlock(self.get_current_workunit())
        for workunit in self.work_units:
            workunit.get_writer().close()


class DirectoryManager(object):
    def __init__(self, directory):
        self.directory = directory

    def get_listing(self, suffix):
        return listdir_for_suffix(self.directory, suffix)

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)


def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))

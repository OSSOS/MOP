__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop.io.persistence import FileLockedException
from pymop.gui import events


class NoAvailableWorkException(Exception):
    """"No more work is available."""


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
    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        self.filename = filename
        self.data = parsed_data
        self.progress_manager = progress_manager
        self.results_writer = results_writer

        self.sources = StatefulCollection(parsed_data.get_sources())

        self.readings_by_source = {}
        for source in self.sources:
            self.readings_by_source[source] = StatefulCollection(source.get_readings())

        self.processed_items = set()

    def get_filename(self):
        return self.filename

    def get_data(self):
        return self.data

    def get_sources(self):
        return self.sources

    def get_source_count(self):
        return len(self.get_sources())

    def get_current_source(self):
        return self.get_sources().get_current_item()

    def get_current_source_number(self):
        return self.get_sources().get_index()

    def get_current_source_readings(self):
        return self.readings_by_source[self.get_current_source()]

    def get_obs_count(self):
        return len(self.get_current_source_readings())

    def get_current_reading(self):
        return self.get_current_source_readings().get_current_item()

    def get_current_obs_number(self):
        return self.get_current_source_readings().get_index()

    def next_source(self):
        self.get_sources().next()
        events.send(events.NEXT_SRC, data=self.get_current_source_number())

    def previous_source(self):
        self.get_sources().previous()
        events.send(events.PREV_SRC, data=self.get_current_source_number())

    def next_obs(self):
        self.get_current_source_readings().next()
        events.send(events.NEXT_OBS, data=self.get_current_obs_number())

    def previous_obs(self):
        self.get_current_source_readings().previous()
        events.send(events.PREV_OBS, data=self.get_current_obs_number())

    def get_current_item(self):
        raise NotImplementedError()

    def accept_current_item(self):
        self.processed_items.add(self.get_current_item())

    def reject_current_item(self):
        self.processed_items.add(self.get_current_item())

    def next_vettable_item(self):
        raise NotImplementedError()

    def is_item_processed(self, item):
        return item in self.processed_items

    def is_current_item_processed(self):
        return self.is_item_processed(self.get_current_item())

    def set_previously_processed(self, indices):
        raise NotImplementedError()

    def get_writer(self):
        return self.results_writer

    def is_finished(self):
        raise NotImplementedError()


class RealsWorkUnit(WorkUnit):
    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(RealsWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def get_current_item(self):
        return self.get_current_reading()

    def next_vettable_item(self):
        # TODO: refactor this to make less complicated...

        # Make sure we are on the right source
        sources_checked = 0
        while (self.is_current_source_finished() and
                       sources_checked < self.get_source_count()):
            sources_checked += 1
            self.next_source()

        # Then proceed to the first unprocessed observation
        if sources_checked == 0:
            self.next_obs()

        num_obs_to_check = len(self.get_current_source_readings())
        while num_obs_to_check > 0:
            if not self.is_current_item_processed():
                # This observation needs to be vetted, stop here
                return

            self.next_obs()
            num_obs_to_check -= 1

    def get_current_item_index(self):
        return (self.get_sources().get_index() * self.get_obs_count() +
                self.get_current_source_readings().get_index())

    def set_previously_processed(self, indices):
        for index in indices:
            source_num = int(index / self.get_obs_count())
            reading_num = index % self.get_obs_count()
            self.processed_items.add(self.get_sources()[source_num].get_readings()[reading_num])

        if self.is_current_item_processed():
            self.next_vettable_item()

    def is_current_source_finished(self):
        for reading in self.get_current_source().get_readings():
            if not self.is_item_processed(reading):
                return False

        return True

    def is_finished(self):
        for source in self.get_sources():
            for reading in source.get_readings():
                if not self.is_item_processed(reading):
                    return False

        return True


class CandidatesWorkUnit(WorkUnit):
    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(CandidatesWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def get_current_item(self):
        return self.get_current_source()

    def next_vettable_item(self):
        self.next_source()

    def get_current_item_index(self):
        return self.get_sources().get_index()

    def set_previously_processed(self, indices):
        for index in indices:
            self.processed_items.add(self.get_sources()[index])

        if self.is_current_item_processed():
            self.next_vettable_item()

    def is_finished(self):
        for source in self.get_sources():
            if not self.is_item_processed(source):
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

            if self.directory_manager.get_file_size(potential_file) == 0:
                continue

            if not self.progress_manager.is_done(potential_file):
                try:
                    self.progress_manager.lock(potential_file)
                except FileLockedException:
                    continue

                return self.builder.build_workunit(
                    self.directory_manager.get_full_path(potential_file))

        raise NoAvailableWorkException()


class WorkUnitBuilder(object):
    def __init__(self, parser, progress_manager, writer_factory):
        self.parser = parser
        self.progress_manager = progress_manager
        self.writer_factory = writer_factory

    def build_workunit(self, full_path):
        parsed_data = self.parser.parse(full_path)

        _, filename = os.path.split(full_path)
        return self._build_workunit(filename, parsed_data,
                                    self.progress_manager,
                                    self.writer_factory.create_writer(
                                        full_path, parsed_data))

    def _build_workunit(self, filename, data, progress_manager, writer):
        raise NotImplementedError()


class RealsWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, progress_manager, writer_factory):
        super(RealsWorkUnitBuilder, self).__init__(
            parser, progress_manager, writer_factory)

    def _build_workunit(self, filename, data, progress_manager, writer):
        return RealsWorkUnit(filename, data, progress_manager, writer)


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, progress_manager, writer_factory):
        super(CandidatesWorkUnitBuilder, self).__init__(
            parser, progress_manager, writer_factory)

    def _build_workunit(self, filename, data, progress_manager, writer):
        return CandidatesWorkUnit(filename, data, progress_manager, writer)


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
            # TODO refactor.
            new_workunit = self.workunit_provider.get_workunit()
            processed_indices = self.progress_manager.get_processed_indices(new_workunit.get_filename())
            new_workunit.set_previously_processed(processed_indices)
            self.work_units.append(new_workunit)

            self.work_units.next()
            events.send(events.NEW_WORK_UNIT)
        else:
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
            filename = self.get_current_workunit().get_filename()
            self.progress_manager.record_done(filename)
            events.send(events.FINISHED_WORKUNIT, filename)
            self.next_workunit()

    def get_num_items_processed(self):
        return self.num_processed

    def is_current_item_processed(self):
        return self.get_current_workunit().is_current_item_processed()

    def get_writer(self):
        return self.get_current_workunit().get_writer()

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

    def get_file_size(self, filename):
        return os.stat(self.get_full_path(filename)).st_size


def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))

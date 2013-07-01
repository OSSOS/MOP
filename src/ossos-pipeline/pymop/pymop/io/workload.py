__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop import tasks
from pymop.gui import events
from pymop.io.mpc import MPCWriter
from pymop.io.astrom import StreamingAstromWriter
from pymop.io.persistence import FileLockedException


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
        self._mark_previously_processed_items()

        if self.is_current_item_processed():
            self.next_item()

        self.finished_callbacks = []

    def register_finished_callback(self, callback):
        self.finished_callbacks.append(callback)

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

    def get_current_item_index(self):
        raise NotImplementedError()

    def accept_current_item(self):
        self.process_current_item()

    def reject_current_item(self):
        self.process_current_item()

    def process_current_item(self):
        self.processed_items.add(self.get_current_item())
        self.progress_manager.record_index(self.get_filename(),
                                           self.get_current_item_index())

        if self.is_finished():
            self.progress_manager.record_done(self.get_filename())
            self.results_writer.close()

            for callback in self.finished_callbacks:
                callback(self.get_filename())

    def next_item(self):
        raise NotImplementedError()

    def is_item_processed(self, item):
        return item in self.processed_items

    def is_current_item_processed(self):
        return self.is_item_processed(self.get_current_item())

    def get_writer(self):
        return self.results_writer

    def is_finished(self):
        raise NotImplementedError()

    def _mark_previously_processed_items(self):
        pass


class RealsWorkUnit(WorkUnit):
    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(RealsWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def get_current_item(self):
        return self.get_current_reading()

    def get_current_item_index(self):
        return (self.get_sources().get_index() * self.get_obs_count() +
                self.get_current_source_readings().get_index())

    def next_item(self):
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

    def _mark_previously_processed_items(self):
        processed_indices = self.progress_manager.get_processed_indices(self.get_filename())
        for index in processed_indices:
            source_num = int(index / self.get_obs_count())
            reading_num = index % self.get_obs_count()
            self.processed_items.add(self.get_sources()[source_num].get_readings()[reading_num])


class CandidatesWorkUnit(WorkUnit):
    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(CandidatesWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def get_current_item(self):
        return self.get_current_source()

    def get_current_item_index(self):
        return self.get_sources().get_index()

    def next_item(self):
        self.next_source()

    def is_finished(self):
        for source in self.get_sources():
            if not self.is_item_processed(source):
                return False

        return True

    def _mark_previously_processed_items(self):
        processed_indices = self.progress_manager.get_processed_indices(self.get_filename())
        for index in processed_indices:
            self.processed_items.add(self.get_sources()[index])


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
    def __init__(self, parser, progress_manager):
        self.parser = parser
        self.progress_manager = progress_manager

    def build_workunit(self, full_path):
        parsed_data = self.parser.parse(full_path)

        _, filename = os.path.split(full_path)
        return self._build_workunit(filename, parsed_data,
                                    self.progress_manager,
                                    self._create_results_writer(full_path,
                                                                parsed_data))

    def _create_results_writer(self, full_path, parsed_data):
        raise NotImplementedError()

    def _build_workunit(self, filename, data, progress_manager, writer):
        raise NotImplementedError()


class RealsWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, progress_manager):
        super(RealsWorkUnitBuilder, self).__init__(parser, progress_manager)

    def _create_results_writer(self, full_path, parsed_data):
        output_filename = full_path.replace(tasks.get_suffix(tasks.REALS_TASK),
                                            ".mpc")
        output_filehandle = open(output_filename, "a+b")
        return MPCWriter(output_filehandle)

    def _build_workunit(self, filename, data, progress_manager, writer):
        return RealsWorkUnit(filename, data, progress_manager, writer)


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser, progress_manager):
        super(CandidatesWorkUnitBuilder, self).__init__(parser, progress_manager)

    def _create_results_writer(self, full_path, parsed_data):
            output_filename = full_path.replace(tasks.get_suffix(tasks.CANDS_TASK),
                                        tasks.get_suffix(tasks.REALS_TASK))
            output_filehandle = open(output_filename, "a+b")
            return StreamingAstromWriter(output_filehandle, parsed_data.sys_header)

    def _build_workunit(self, filename, data, progress_manager, writer):
        return CandidatesWorkUnit(filename, data, progress_manager, writer)

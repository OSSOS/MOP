__author__ = "David Rusk <drusk@uvic.ca>"

import os

from ossos.gui import tasks
from ossos.mpc import MPCWriter
from ossos.astrom import StreamingAstromWriter
from ossos.gui.persistence import FileLockedException


class NoAvailableWorkException(Exception):
    """"No more work is available."""


class StatefulCollection(object):
    """
    An ordered collection of objects which have the notion of one of them
    being the 'current' object.
    """

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

    def register_change_item_callback(self, callback):
        """
        Registers a function to be called when the current item is changed.
        The function will be called with two arguments: the item that was
        current, and the new current item.
        """
        self.callbacks.append(callback)

    def append(self, item):
        """Adds a new item to the end of the collection."""
        if len(self) == 0:
            # Special case, we make this the current item
            self.index = 0

        self.items.append(item)

    def get_index(self):
        """Returns the index of the current item."""
        return self.index

    def get_current_item(self):
        if self.items:
            return self.items[self.index]
        else:
            return None

    def next(self):
        """
        Make the next item in the collection the current item.  Wraps around
        to the beginning after reaching the end.
        """
        self._move(1)

    def previous(self):
        """
        Make the previous item in the collection the current item.  Wraps
        around to the end after reaching the beginning.
        """
        self._move(-1)

    def is_on_last_item(self):
        """
        Returns True if the current item is the last item in the collection.
        """
        return self.index == len(self) - 1

    def _move(self, delta):
        first_item = self.get_current_item()
        self.index = (self.index + delta) % len(self)
        second_item = self.get_current_item()

        for callback in self.callbacks:
            callback(first_item, second_item)


class WorkUnit(object):
    """
    A unit of work to be processed, associated with the data from a single
    input file.
    """

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

    def next_source(self):
        self.get_sources().next()

    def previous_source(self):
        self.get_sources().previous()

    def next_obs(self):
        self.get_current_source_readings().next()

    def previous_obs(self):
        self.get_current_source_readings().previous()

    def next_item(self):
        raise NotImplementedError()

    def accept_current_item(self):
        self.process_current_item()

    def reject_current_item(self):
        self.process_current_item()

    def process_current_item(self):
        self.processed_items.add(self.get_current_item())

        if self.is_current_source_finished():
            self.progress_manager.record_index(self.get_filename(),
                                               self.get_current_source_number())

        if self.is_finished():
            self.progress_manager.record_done(self.get_filename())

            for callback in self.finished_callbacks:
                callback(self.get_filename())

    def get_current_item(self):
        raise NotImplementedError()

    def get_current_item_index(self):
        raise NotImplementedError()

    def is_item_processed(self, item):
        return item in self.processed_items

    def is_current_item_processed(self):
        return self.is_item_processed(self.get_current_item())

    def is_current_source_finished(self):
        raise NotImplementedError()

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

    def get_writer(self):
        return self.results_writer

    def is_finished(self):
        return len(self._get_item_set() - self.processed_items) == 0

    def _get_item_set(self):
        raise NotImplementedError()

    def _mark_previously_processed_items(self):
        pass


class RealsWorkUnit(WorkUnit):
    """
    A unit of work when performing the process reals task.
    """

    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(RealsWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def next_item(self):
        self.next_obs()
        while self.is_current_item_processed():
            self._next_sequential_item()

    def _next_sequential_item(self):
        """
        Go to the next item in the 'ideal' processing sequence.
        """
        if self.get_current_source_readings().is_on_last_item():
            self.next_source()
        else:
            self.next_obs()

    def get_current_item(self):
        return self.get_current_reading()

    def get_current_item_index(self):
        return (self.get_sources().get_index() * self.get_obs_count() +
                self.get_current_source_readings().get_index())

    def is_current_source_finished(self):
        for reading in self.get_current_source_readings():
            if reading not in self.processed_items:
                return False

        return True

    def _get_item_set(self):
        all_readings = set()
        for readings in self.readings_by_source.itervalues():
            all_readings.update(readings)
        return all_readings

    def _mark_previously_processed_items(self):
        processed_indices = self.progress_manager.get_processed_indices(self.get_filename())
        for index in processed_indices:
            for reading in self.get_sources()[index].get_readings():
                self.processed_items.add(reading)


class CandidatesWorkUnit(WorkUnit):
    """
    A unit of work when performing the process candidates task.
    """

    def __init__(self, filename, parsed_data, progress_manager, results_writer):
        super(CandidatesWorkUnit, self).__init__(
            filename, parsed_data, progress_manager, results_writer)

    def next_item(self):
        self.next_source()

    def get_current_item(self):
        return self.get_current_source()

    def get_current_item_index(self):
        return self.get_sources().get_index()

    def is_current_source_finished(self):
        return self.get_current_source() in self.processed_items

    def _get_item_set(self):
        return set(self.sources)

    def _mark_previously_processed_items(self):
        processed_indices = self.progress_manager.get_processed_indices(self.get_filename())
        for index in processed_indices:
            self.processed_items.add(self.get_sources()[index])


class WorkUnitProvider(object):
    """
    Obtains new units of work for the application.
    """

    def __init__(self,
                 taskid,
                 directory_context,
                 progress_manager,
                 builder):
        self.taskid = taskid
        self.directory_context = directory_context
        self.progress_manager = progress_manager
        self.builder = builder

    def get_workunit(self):
        """
        Gets a new unit of work.

        Returns:
          new_workunit: WorkUnit
            A new unit of work that has not yet been processed.

        Raises:
          NoAvailableWorkException
            There is no more work available.
        """
        potential_files = self.directory_context.get_listing(self.taskid)

        while len(potential_files) > 0:
            potential_file = potential_files.pop()

            if self.directory_context.get_file_size(potential_file) == 0:
                continue

            if not self.progress_manager.is_done(potential_file):
                try:
                    self.progress_manager.lock(potential_file)
                except FileLockedException:
                    continue

                return self.builder.build_workunit(
                    self.directory_context.get_full_path(potential_file))

        raise NoAvailableWorkException()


class WorkUnitBuilder(object):
    """
    Used to construct a WorkUnit with its necessary components.
    """

    def __init__(self, parser, progress_manager):
        self.parser = parser
        self.progress_manager = progress_manager

    def build_workunit(self, full_path):
        parsed_data = self.parser.parse(full_path)

        _, filename = os.path.split(full_path)
        return self._do_build_workunit(filename, parsed_data,
                                       self.progress_manager,
                                       self._create_results_writer(full_path,
                                                                   parsed_data))

    def _create_results_writer(self, full_path, parsed_data):
        raise NotImplementedError()

    def _do_build_workunit(self, filename, data, progress_manager, writer):
        raise NotImplementedError()


class RealsWorkUnitBuilder(WorkUnitBuilder):
    """
    Used to construct a WorkUnit with its necessary components.
    Constructs RealsWorkUnits for the process reals task.
    """

    def __init__(self, parser, progress_manager):
        super(RealsWorkUnitBuilder, self).__init__(parser, progress_manager)

    def _create_results_writer(self, full_path, parsed_data):
        output_filename = full_path.replace(tasks.get_suffix(tasks.REALS_TASK),
                                            ".mpc")
        output_filehandle = open(output_filename, "a+b")
        return MPCWriter(output_filehandle, auto_flush=False)

    def _do_build_workunit(self, filename, data, progress_manager, writer):
        return RealsWorkUnit(filename, data, progress_manager, writer)


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    """
    Used to construct a WorkUnit with its necessary components.
    Constructs CandidatesWorkUnits for the process candidates task.
    """

    def __init__(self, parser, progress_manager):
        super(CandidatesWorkUnitBuilder, self).__init__(parser, progress_manager)

    def _create_results_writer(self, full_path, parsed_data):
        output_filename = full_path.replace(tasks.get_suffix(tasks.CANDS_TASK),
                                            tasks.get_suffix(tasks.REALS_TASK))
        output_filehandle = open(output_filename, "a+b")
        return StreamingAstromWriter(output_filehandle, parsed_data.sys_header)

    def _do_build_workunit(self, filename, data, progress_manager, writer):
        return CandidatesWorkUnit(filename, data, progress_manager, writer)

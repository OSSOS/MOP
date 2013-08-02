__author__ = "David Rusk <drusk@uvic.ca>"

import os
import random
import threading

from ossos.gui import tasks, logger
from ossos.astrom import StreamingAstromWriter
from ossos.gui.persistence import FileLockedException


class NoAvailableWorkException(Exception):
    """"No more work is available."""


class SourceNotNamedException(Exception):
    """The source has no name."""
    def __init__(self, source):
        self.source = source


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

        self._frozen = False

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def __getitem__(self, index):
        return self.items[index]

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

    def freeze(self):
        """
        Causes requests for transitions between items to be ignored. Call
        unfreeze to re-enable transitions.
        """
        self._frozen = True

    def unfreeze(self):
        """
        Re-enables transitions between items after being frozen.
        """
        self._frozen = False

    def _move(self, delta):
        if self._frozen:
            return

        self.index = (self.index + delta) % len(self)


class WorkUnit(object):
    """
    A unit of work to be processed, associated with the data from a single
    input file.
    """

    def __init__(self, filename,
                 parsed_data,
                 progress_manager,
                 output_context):
        self.filename = filename
        self.data = parsed_data
        self.progress_manager = progress_manager
        self.output_context = output_context

        self.sources = StatefulCollection(parsed_data.get_sources())

        self.readings_by_source = {}
        for source in self.sources:
            self.readings_by_source[source] = StatefulCollection(source.get_readings())

        self.processed_items = set()
        self._mark_previously_processed_items()

        if self.is_current_item_processed():
            self.next_item()

        self.finished_callbacks = []

        self._unlocked = False

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
            self.get_writer().flush()

        if self.is_finished():
            self.progress_manager.record_done(self.get_filename())
            self.unlock()

            for callback in self.finished_callbacks:
                callback(self.get_results_file_paths())

            self._close_writers()

    def get_current_item(self):
        raise NotImplementedError()

    def get_current_item_index(self):
        raise NotImplementedError()

    def is_item_processed(self, item):
        return item in self.processed_items

    def is_current_item_processed(self):
        return self.is_item_processed(self.get_current_item())

    def is_source_finished(self, source):
        raise NotImplementedError()

    def is_current_source_finished(self):
        return self.is_source_finished(self.get_current_source())

    def get_filename(self):
        return self.filename

    def get_data(self):
        return self.data

    def get_sources(self):
        return self.sources

    def get_unprocessed_sources(self):
        unprocessed_sources = []
        for source in self.get_sources():
            if not self.is_source_finished(source):
                unprocessed_sources.append(source)

        return unprocessed_sources

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
        raise NotImplementedError()

    def get_results_file_paths(self):
        raise NotImplementedError()

    def is_finished(self):
        return len(self._get_item_set() - self.processed_items) == 0

    def is_apcor_needed(self):
        raise NotImplementedError()

    def unlock(self):
        if not self._unlocked:
            self.progress_manager.unlock(self.get_filename(), async=True)
            self._unlocked = True

    def freeze(self):
        self.sources.freeze()
        self.get_current_source_readings().freeze()

    def unfreeze(self):
        self.sources.unfreeze()
        self.get_current_source_readings().unfreeze()

    def _get_item_set(self):
        raise NotImplementedError()

    def _mark_previously_processed_items(self):
        pass

    def _close_writers(self):
        pass


class RealsWorkUnit(WorkUnit):
    """
    A unit of work when performing the process reals task.
    """

    def __init__(self,
                 filename,
                 parsed_data,
                 progress_manager,
                 output_context):
        super(RealsWorkUnit, self).__init__(
            filename,
            parsed_data,
            progress_manager,
            output_context)

        self._writers = {}

    def next_item(self):
        assert not self.is_finished()

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

    def is_source_finished(self, source):
        for reading in source.get_readings():
            if reading not in self.processed_items:
                return False

        return True

    def is_apcor_needed(self):
        return True

    def get_writer(self):
        filename = self._get_current_source_output_filename()
        if filename in self._writers:
            return self._writers[filename]

        writer = self._create_writer(filename)
        self._writers[filename] = writer
        return writer

    def get_results_file_paths(self):
        return [self.output_context.get_full_path(filename)
                for filename in self._writers]

    def _get_current_source_output_filename(self):
        source = self.get_current_source()

        if not source.has_provisional_name():
            raise SourceNotNamedException(source)

        return source.get_provisional_name() + ".mpc"

    def _create_writer(self, filename):
        # NOTE: this import is only here so that we don't load up secondary
        # dependencies (like astropy) used in MPCWriter when they are not
        # needed (i.e. cands task).  This is to help reduce the application
        # startup time.
        from ossos.mpc import MPCWriter

        return MPCWriter(self.output_context.open(filename),
                         auto_flush=False)

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

    def _close_writers(self):
        for writer in self._writers.values():
            writer.close()


class CandidatesWorkUnit(WorkUnit):
    """
    A unit of work when performing the process candidates task.
    """

    def __init__(self,
                 filename,
                 parsed_data,
                 progress_manager,
                 output_context):
        super(CandidatesWorkUnit, self).__init__(
            filename,
            parsed_data,
            progress_manager,
            output_context)

        output_filename = filename.replace(tasks.get_suffix(tasks.CANDS_TASK),
                                           tasks.get_suffix(tasks.REALS_TASK))
        self._writer = self._create_writer(output_filename)

    def next_item(self):
        assert not self.is_finished()

        self.next_source()
        while self.is_current_item_processed():
            self.next_source()

    def get_current_item(self):
        return self.get_current_source()

    def get_current_item_index(self):
        return self.get_sources().get_index()

    def is_source_finished(self, source):
        return source in self.processed_items

    def is_apcor_needed(self):
        return False

    def get_writer(self):
        return self._writer

    def get_results_file_paths(self):
        return [self.output_context.get_full_path(self._writer.get_filename())]

    def _create_writer(self, filename):
        return StreamingAstromWriter(self.output_context.open(filename),
                                     self.data.sys_header)

    def _get_item_set(self):
        return set(self.sources)

    def _mark_previously_processed_items(self):
        processed_indices = self.progress_manager.get_processed_indices(self.get_filename())
        for index in processed_indices:
            self.processed_items.add(self.get_sources()[index])

    def _close_writers(self):
        self._writer.close()


class WorkUnitProvider(object):
    """
    Obtains new units of work for the application.
    """

    def __init__(self,
                 taskid,
                 directory_context,
                 progress_manager,
                 builder,
                 randomize=False):
        self.taskid = taskid
        self.directory_context = directory_context
        self.progress_manager = progress_manager
        self.builder = builder
        self.randomize = randomize

        self._done = []
        self._already_fetched = []

    @property
    def directory(self):
        """
        The directory that workunits are being acquired from.
        """
        return self.directory_context.directory

    def get_workunit(self, ignore_list=None):
        """
        Gets a new unit of work.

        Args:
          ignore_list: list(str)
            A list of filenames which should be ignored.  Defaults to None.

        Returns:
          new_workunit: WorkUnit
            A new unit of work that has not yet been processed.  A lock on
            it has been acquired.

        Raises:
          NoAvailableWorkException
            There is no more work available.
        """
        if ignore_list is None:
            ignore_list = []

        potential_files = self.get_potential_files(ignore_list)

        while len(potential_files) > 0:
            potential_file = self.select_potential_file(potential_files)
            potential_files.remove(potential_file)

            if self.directory_context.get_file_size(potential_file) == 0:
                continue

            if self.progress_manager.is_done(potential_file):
                self._done.append(potential_file)
                continue
            else:
                try:
                    self.progress_manager.lock(potential_file)
                except FileLockedException:
                    continue

                self._already_fetched.append(potential_file)

                return self.builder.build_workunit(
                    self.directory_context.get_full_path(potential_file))

        logger.info("No eligible workunits remain to be fetched.")

        raise NoAvailableWorkException()

    def get_potential_files(self, ignore_list):
        """
        Get a listing of files for the appropriate task which may or may
        not be locked and/or done.
        """
        return [file for file in self.directory_context.get_listing(self.taskid)
                if file not in ignore_list and
                   file not in self._done and
                   file not in self._already_fetched]

    def select_potential_file(self, potential_files):
        if self.randomize:
            # Don't want predictable patterns in the order we get work units.
            return random.choice(potential_files)
        else:
            return potential_files[0]

    def shutdown(self):
        pass


class PreFetchingWorkUnitProvider(object):
    def __init__(self, workunit_provider, prefetch_quantity):
        self.workunit_provider = workunit_provider
        self.prefetch_quantity = prefetch_quantity

        self.fetched_files = []
        self.workunits = []

        self._threads = []
        self._all_fetched = False

    @property
    def directory(self):
        """
        The directory that workunits are being acquired from.
        """
        return self.workunit_provider.directory

    def get_workunit(self):
        if self._all_fetched and len(self.workunits) == 0:
            raise NoAvailableWorkException()

        if len(self.workunits) > 0:
            workunit = self.workunits.pop(0)
        else:
            workunit = self.workunit_provider.get_workunit(
                ignore_list=self.fetched_files)
            self.fetched_files.append(workunit.get_filename())

        self.trigger_prefetching()
        return workunit

    def trigger_prefetching(self):
        if self._all_fetched:
            return

        num_to_fetch = self.prefetch_quantity - len(self.workunits)

        if num_to_fetch < 0:
            # Prefetch quantity can be 0
            num_to_fetch = 0

        while num_to_fetch > 0:
            self.prefetch_workunit()
            num_to_fetch -= 1

    def prefetch_workunit(self):
        thread = threading.Thread(target=self._do_prefetch_workunit)
        self._threads.append(thread)
        thread.start()

    def _do_prefetch_workunit(self):
        try:
            workunit = self.workunit_provider.get_workunit(
                ignore_list=self.fetched_files)
            filename = workunit.get_filename()

            self.fetched_files.append(filename)
            self.workunits.append(workunit)

            logger.info("%s was prefetched." % filename)

        except NoAvailableWorkException:
            self._all_fetched = True

    def shutdown(self):
        # Make sure all threads are finished so that no more locks are
        # acquired
        for thread in self._threads:
            thread.join()

        for workunit in self.workunits:
            workunit.unlock()


class WorkUnitBuilder(object):
    """
    Used to construct a WorkUnit with its necessary components.
    """

    def __init__(self, parser, input_context, output_context, progress_manager):
        self.parser = parser
        self.input_context = input_context
        self.output_context = output_context
        self.progress_manager = progress_manager

    def build_workunit(self, input_fullpath):
        parsed_data = self.parser.parse(input_fullpath)

        logger.debug("Parsed %s (%d sources)" %
                     (input_fullpath, parsed_data.get_source_count()))

        _, input_filename = os.path.split(input_fullpath)

        return self._do_build_workunit(
            input_filename,
            parsed_data,
            self.progress_manager,
            self.output_context)

    def _do_build_workunit(self,
                           filename,
                           data,
                           progress_manager,
                           output_context):
        raise NotImplementedError()


class RealsWorkUnitBuilder(WorkUnitBuilder):
    """
    Used to construct a WorkUnit with its necessary components.
    Constructs RealsWorkUnits for the process reals task.
    """

    def __init__(self, parser, input_context, output_context, progress_manager):
        super(RealsWorkUnitBuilder, self).__init__(
            parser, input_context, output_context, progress_manager)

    def _do_build_workunit(self,
                           filename,
                           data,
                           progress_manager,
                           output_context):
        return RealsWorkUnit(
            filename, data, progress_manager, output_context)


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    """
    Used to construct a WorkUnit with its necessary components.
    Constructs CandidatesWorkUnits for the process candidates task.
    """

    def __init__(self, parser, input_context, output_context, progress_manager):
        super(CandidatesWorkUnitBuilder, self).__init__(
            parser, input_context, output_context, progress_manager)

    def _do_build_workunit(self,
                           filename,
                           data,
                           progress_manager,
                           output_context):
        return CandidatesWorkUnit(
            filename, data, progress_manager, output_context)

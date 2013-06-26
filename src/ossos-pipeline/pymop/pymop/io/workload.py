__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop.io.persistence import FileLockedException


class NoAvailableWorkException(Exception):
    """"No more work is available."""


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


class WorkUnit(object):
    def __init__(self, filename, work_items):
        self.filename = filename
        self.work_items = work_items

    def get_filename(self):
        return self.filename

    def get_work_items(self):
        return self.work_items


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


class WorkUnitBuilder(object):
    def __init__(self, parser):
        self.parser = parser

    def build_workunit(self, full_path):
        work_items = self._create_work_items(self.parser.parse(full_path))
        _, filename = os.path.split(full_path)
        return WorkUnit(filename, work_items)

    def _create_work_items(self, parsed_data):
        raise NotImplementedError()


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser):
        super(CandidatesWorkUnitBuilder, self).__init__(parser)

    def _create_work_items(self, parsed_data):
        return [VettableItem(source) for source in parsed_data.get_sources()]


class RealsWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser):
        super(RealsWorkUnitBuilder, self).__init__(parser)

    def _create_work_items(self, parsed_data):
        return [VettableItem(reading) for source in parsed_data.get_sources()
                for reading in source]


class WorkloadManager(object):
    """
    Manages the workload's state.
    """

    def __init__(self, workunit_factory):
        self.workunit_factory = workunit_factory
        self.current_workunit = None
        self.workunit_number = 0

    def next_workunit(self):
        pass

    def previous_workunit(self):
        pass

    def get_current_filename(self):
        pass

    def get_current_data(self):
        pass


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

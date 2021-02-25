__author__ = "David Rusk <drusk@uvic.ca>"

import os

from ossos import storage
from ossos.gui import logger
from ossos.gui.progress import LocalProgressManager, VOSpaceProgressManager


def get_context(directory, userid=None):
    if directory.startswith("vos:"):
        return VOSpaceWorkingContext(directory, userid=userid)
    else:
        return LocalDirectoryWorkingContext(directory, userid=userid)


class WorkingContext(object):
    def __init__(self, directory, userid=None):
        self.userid = userid
        self.directory = directory

    def is_remote(self):
        raise NotImplementedError()

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)

    def listdir(self):
        raise NotImplementedError()

    def get_listing(self, suffix, exclude_prefix=None):
        if not exclude_prefix:
            name_match = lambda x: x.endswith(suffix)
        else:
            name_match = lambda x: x.endswith(suffix) and not x.startswith(exclude_prefix)
        return list(filter(name_match, self.listdir()))

    def get_file_size(self, filename):
        raise NotImplementedError()

    def exists(self, filename):
        raise NotImplementedError()

    def open(self, filename):
        raise NotImplementedError()

    def rename(self, old_name, new_name):
        raise NotImplementedError()

    def remove(self, filename):
        raise NotImplementedError()

    def get_progress_manager(self):
        raise NotImplementedError()


class LocalDirectoryWorkingContext(WorkingContext):
    def __init__(self, directory, userid=None):
        super(LocalDirectoryWorkingContext, self).__init__(directory, userid=userid)

    def is_remote(self):
        return False

    def listdir(self):
        return os.listdir(self.directory)

    def get_file_size(self, filename):
        return os.stat(self.get_full_path(filename)).st_size

    def exists(self, filename):
        return os.path.exists(self.get_full_path(filename))

    def open(self, filename):
        filehandle = open(self.get_full_path(filename), 'a+t')

        # Note: Linux starts the file position at 0, but on MacOSX it
        # starts at the end of the file, so this makes things consistent
        # and allows us to subsequently call a read with the expected
        # results.
        filehandle.seek(0)

        return filehandle

    def rename(self, old_name, new_name):
        os.rename(self.get_full_path(old_name), self.get_full_path(new_name))

    def remove(self, filename):
        os.remove(self.get_full_path(filename))

    def get_progress_manager(self):
        return LocalProgressManager(self, userid=self.userid)


class VOSpaceWorkingContext(WorkingContext):
    def __init__(self, directory, userid=None):
        super(VOSpaceWorkingContext, self).__init__(directory, userid=userid)

    def is_remote(self):
        return True

    def listdir(self):
        # Don't force because it causes a HUGE performance hit.
        logger.debug(f"Getting a listing of {self.directory}")
        dir_list = storage.listdir(self.directory, force=False)
        logger.debug(f"Got: {dir_list}")
        return dir_list

    def get_file_size(self, filename):
        length_property = storage.get_property(
            self.get_full_path(filename), "length", ossos_base=False)

        if length_property is None:
            # Fall-back if the length property is not set for some reason
            return self._get_file_size_by_downloading(filename)

        return int(length_property)

    def _get_file_size_by_downloading(self, filename):
        filehandle = storage.vofile(self.get_full_path(filename), os.O_RDONLY)
        contents = filehandle.read()
        filehandle.close()
        return len(contents)

    def exists(self, filename):
        return storage.exists(self.get_full_path(filename))

    def open(self, filename):
        return storage.vofile(self.get_full_path(filename), os.O_RDONLY)

    def rename(self, old_name, new_name):
        storage.move(old_name, new_name)

    def remove(self, filename):
        storage.delete_uri(self.get_full_path(filename))

    def get_progress_manager(self):
        return VOSpaceProgressManager(self, track_partial_progress=False, userid=self.userid)

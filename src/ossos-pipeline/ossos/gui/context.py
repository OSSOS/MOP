__author__ = "David Rusk <drusk@uvic.ca>"

import os

from ossos import storage


def get_context(directory):
    if directory.startswith("vos:"):
        return VOSpaceWorkingContext(directory)
    else:
        return LocalDirectoryWorkingContext(directory)


class WorkingContext(object):
    def __init__(self, directory):
        self.directory = directory

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)

    def listdir(self):
        raise NotImplementedError()

    def get_listing(self, suffix):
        return filter(lambda name: name.endswith(suffix), self.listdir())

    def get_file_size(self, filename):
        raise NotImplementedError()

    def exists(self, filename):
        raise NotImplementedError()

    def open(self, filename, mode):
        return storage.open_vos_or_local(self.get_full_path(filename), mode)

    def rename(self, old_name, new_name):
        raise NotImplementedError()

    def remove(self, filename):
        raise NotImplementedError()


class LocalDirectoryWorkingContext(WorkingContext):
    def __init__(self, directory):
        super(LocalDirectoryWorkingContext, self).__init__(directory)

    def listdir(self):
        return os.listdir(self.directory)

    def get_file_size(self, filename):
        return os.stat(self.get_full_path(filename)).st_size

    def exists(self, filename):
        return os.path.exists(self.get_full_path(filename))

    def rename(self, old_name, new_name):
        os.rename(self.get_full_path(old_name), self.get_full_path(new_name))

    def remove(self, filename):
        os.remove(self.get_full_path(filename))


class VOSpaceWorkingContext(WorkingContext):
    def __init__(self, directory):
        super(VOSpaceWorkingContext, self).__init__(directory)

    def listdir(self):
        return storage.listdir(self.directory)

    def get_file_size(self, filename):
        # TODO: better/faster way of doing this?
        filehandle = self.open(filename, "rb")
        contents = filehandle.read()
        filehandle.close()

        return len(contents)

    def exists(self, filename):
        return storage.exists(self.get_full_path(filename))

    def rename(self, old_name, new_name):
        storage.move(old_name, new_name)

    def remove(self, filename):
        storage.delete_uri(self.get_full_path(filename))


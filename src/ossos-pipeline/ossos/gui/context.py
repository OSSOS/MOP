__author__ = "David Rusk <drusk@uvic.ca>"

import os


class LocalDirectoryWorkingContext(object):
    def __init__(self, directory):
        self.directory = directory

    def get_listing(self, suffix):
        return listdir_for_suffix(self.directory, suffix)

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)

    def get_file_size(self, filename):
        return os.stat(self.get_full_path(filename)).st_size

    def exists(self, filename):
        return os.path.exists(self.get_full_path(filename))


def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))

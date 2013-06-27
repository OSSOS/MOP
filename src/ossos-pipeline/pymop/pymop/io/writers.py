__author__ = "David Rusk <drusk@uvic.ca>"

from pymop import tasks
from pymop.io.mpc import MPCWriter
from pymop.io.astrom import StreamingAstromWriter


class UnrecognizedExtensionError(Exception):
    """An error indicating an unrecognized file extension was received."""


class WriterFactory(object):
    def create_writer(self, full_path, data):
        if full_path.endswith(tasks.get_suffix(tasks.CANDS_TASK)):
            output_filename = full_path.replace(tasks.get_suffix(tasks.CANDS_TASK),
                                                tasks.get_suffix(tasks.REALS_TASK))
            output_filehandle = open(output_filename, "wb")
            return StreamingAstromWriter(output_filehandle, data.sys_header)

        elif full_path.endswith(tasks.get_suffix(tasks.REALS_TASK)):
            output_filename = full_path.replace(tasks.get_suffix(tasks.REALS_TASK),
                                                ".mpc")
            output_filehandle = open(output_filename, "ab")
            return MPCWriter(output_filehandle)

        else:
            raise UnrecognizedExtensionError()

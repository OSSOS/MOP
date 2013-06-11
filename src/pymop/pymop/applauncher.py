"""
Performs necessary application startup tasks.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from pymop.io.astrom import AstromParser
from pymop.io.mpc import MPCWriter
from pymop.io.naming import ProvisionalNameGenerator
from pymop.io.imgaccess import (AsynchronousImageDownloadManager,
                                ImageSliceDownloader, VOSpaceResolver)
from pymop.gui.models import ProcessRealsModel
from pymop.gui.controllers import ApplicationController


class AstromFileApplicationLauncher(object):
    """
    Launches the application given the path to a .astrom file.
    """

    def __init__(self):
        self.parser = AstromParser()
        self.name_generator = ProvisionalNameGenerator()

        self.download_manager = AsynchronousImageDownloadManager(
            ImageSliceDownloader(VOSpaceResolver()))

    def run(self, astrom_file, output_filename, debug_mode, unittest=False):
        with open(output_filename, "wb") as output_filehandle:
            self.astrom_data = self.parser.parse(astrom_file)

            self.model = ProcessRealsModel(self.astrom_data, self.download_manager)
            self.output_writer = MPCWriter(output_filehandle)
            self.controller = ApplicationController(self.model,
                                                    self.output_writer,
                                                    self.name_generator,
                                                    debug_mode=debug_mode,
                                                    unittest=unittest)
        return self.controller


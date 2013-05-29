"""
Performs necessary application startup tasks.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from pymop.io.parser import AstromParser
from pymop.io.writer import MPCWriter
from pymop.io.naming import ProvisionalNameGenerator
from pymop.io.resolver import VOSpaceResolver
from pymop.io.loader import AsynchronousImageLoader
from pymop.io.image_retriever import ImageSliceRetriever
from pymop.model.astrodata import AstroDataModel
from pymop.controller.appcontrol import ApplicationController
from pymop.view.image.ds9view import DS9ImageViewer


class AstromFileApplicationLauncher(object):
    """
    Launches the application given the path to a .astrom file.
    """

    def __init__(self):
        self.parser = AstromParser()
        self.resolver = VOSpaceResolver()
        self.image_retriever = ImageSliceRetriever()
        self.image_viewer = DS9ImageViewer()
        self.name_generator = ProvisionalNameGenerator()

        self.image_loader = AsynchronousImageLoader(self.resolver, self.image_retriever)

    def run(self, astrom_file, output_filehandle, debug_mode, unittest=False):
        self.astrom_data = self.parser.parse(astrom_file)

        self.model = AstroDataModel(self.astrom_data, self.image_loader)
        self.output_writer = MPCWriter(output_filehandle)
        self.appcontroller = ApplicationController(self.model,
                                                   self.output_writer,
                                                   self.name_generator,
                                                   self.image_viewer,
                                                   debug_mode=debug_mode,
                                                   unittest=unittest)
        return self.appcontroller


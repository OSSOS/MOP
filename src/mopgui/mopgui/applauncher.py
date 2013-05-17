"""
Performs necessary application startup tasks.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

from mopgui.io.parser import AstromParser
from mopgui.io.writer import AcceptRejectResultsWriter
from mopgui.io.naming import ProvisionalNameGenerator
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.data_retrieval.loader import AsynchronousImageLoader
from mopgui.data_retrieval.image_retriever import ImageSliceRetriever
from mopgui.model.astrodata import AstroDataModel
from mopgui.view.imageview import DS9ImageViewer
from mopgui.controller.appcontrol import ApplicationController


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
        self.output_writer = AcceptRejectResultsWriter(output_filehandle,
                                                       self.name_generator)
        self.appcontroller = ApplicationController(self.model,
                                                   self.output_writer,
                                                   self.name_generator,
                                                   self.image_viewer,
                                                   debug_mode=debug_mode,
                                                   unittest=unittest)
        return self.appcontroller


"""
Performs necessary application startup tasks.
"""

from mopgui.io.parser import AstromParser
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.data_retrieval.loader import ProgressiveImageLoader
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

        self.image_loader = ProgressiveImageLoader(self.resolver, self.image_retriever)

    def run(self, astrom_file, debug_mode):
        self.astrom_data = self.parser.parse(astrom_file)

        self.model = AstroDataModel(self.astrom_data)

        def print_read():
            print "Read image"

        self.image_loader.start_loading(self.astrom_data, print_read)

        self.appcontroller = ApplicationController(self.model,
                                                   self.image_viewer,
                                                   debug_mode=debug_mode)

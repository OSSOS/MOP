"""
Main controller of the application.
"""

from mopgui.parsing.parser import AstromParser
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.data_retrieval.image_retriever import ImageSliceRetriever
from mopgui.view.appview import ApplicationView
from mopgui.view.imageview import DS9ImageViewer


class ApplicationController(object):
    """
    Controls and coordinates the various components of the application.
    """

    def __init__(self):
        self.parser = AstromParser()
        self.resolver = VOSpaceResolver()
        self.image_retriever = ImageSliceRetriever()
        self.image_viewer = DS9ImageViewer()

    def run(self, astrom_file, debug_mode):
        self.astrom_data = self.parser.parse(astrom_file)

        # Load all image slices up front for now
        for source in self.astrom_data.sources:
            for reading in source:
                image_uri = self.resolver.resolve_uri(reading.obs)
                image, _ = self.image_retriever.retrieve_image(image_uri, reading)

                assert image is not None, \
                    "No image retrieved for source reading %s" % reading

                reading.image = image
                print "Read image"

        self.view = ApplicationView(self.astrom_data,
                                    self.image_viewer).launch(debug_mode)

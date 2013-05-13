"""
Main controller of the application.
"""

from mopgui.io.parser import AstromParser
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.data_retrieval.image_retriever import ImageSliceRetriever
from mopgui.view.appview import ApplicationView
from mopgui.view.imageview import DS9ImageViewer
from mopgui.model.astrodata import AstroDataModel


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
                image, converter = self.image_retriever.retrieve_image(
                    image_uri, reading)

                assert image is not None, \
                    "No image retrieved for source reading %s" % reading

                reading.image = image
                reading.converter = converter
                print "Read image"

        self.model = AstroDataModel(self.astrom_data)

        self.view = ApplicationView(self.model,
                                    self.image_viewer).launch(debug_mode)


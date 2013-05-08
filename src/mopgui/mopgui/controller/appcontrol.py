"""
Main controller of the application.
"""

from mopgui.parsing.parser import AstromParser
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.data_retrieval.image_retriever import ImageSliceRetriever
from mopgui.view.appview import ApplicationView


class ApplicationController(object):
    def __init__(self):
        self.parser = AstromParser()
        self.resolver = VOSpaceResolver()
        self.image_retriever = ImageSliceRetriever()

    def run(self, astrom_file, debug_mode):
        astrom_data = self.parser.parse(astrom_file)

        # TODO load all image slices up front for now
        for source in astrom_data.sources:
            for reading in source:
                image_uri = self.resolver.resolve_uri(reading.obs)
                image = self.image_retriever.retrieve_image(image_uri, reading)

                assert image is not None, \
                    "No image retrieved for source reading %s" % reading

                reading.image = image

        self.view = ApplicationView(astrom_data).launch(debug_mode)

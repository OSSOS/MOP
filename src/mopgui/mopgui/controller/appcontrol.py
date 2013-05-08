"""
Main controller of the application.
"""

from mopgui.parsing.parser import AstromParser
from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.view.appview import ApplicationView


class ApplicationController(object):
    def __init__(self):
        self.parser = AstromParser()
        self.resolver = VOSpaceResolver()

    def run(self, astrom_file, debug_mode):
        astrom_data = self.parser.parse(astrom_file)

        print "Number of sources: %d" % len(astrom_data.sources)
        for source in astrom_data.sources:
            for reading in source:
                print self.resolver.resolve_uri(reading.obs)

        # TODO load all image slices up front for now

        self.view = ApplicationView(astrom_data).launch(debug_mode)

"""
Main module for running the MOP graphical user interface.
"""

import argparse

from mopgui.parsing.parser import AstromParser
from mopgui.view import appview


def run(astrom_file, debug_mode):
    astrom_data = AstromParser().parse(astrom_file)

    print "Number of sources: %d" % len(astrom_data.sources)
    for source in astrom_data.sources:
        print source

    appview.ApplicationView().launch(debug_mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("astrom_file",
                        help="the .astrom file specifying the images to be analyzed")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="start the system in debug mode")

    args = parser.parse_args()

    run(args.astrom_file, args.debug)

"""
Main module for running the MOP graphical user interface.
"""

import argparse

from mopgui.view import appview


def run(astrom_file, debug_mode):
    print "astrom_file: %s" % astrom_file
    print "debug_mode: %s" % debug_mode

    # TODO: actually run the application
    appview.ApplicationView().launch(debug_mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("astrom_file",
                        help="the .astrom file specifying the images to be analyzed")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="start the system in debug mode")

    args = parser.parse_args()

    run(args.astrom_file, args.debug)

"""
Script for launching the MOP application.
"""

import argparse

from mopgui.controller.appcontrol import ApplicationController


def run(astrom_file, debug_mode):
    ApplicationController().run(astrom_file, debug_mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("astrom_file",
                        help="the .astrom file specifying the images to be analyzed")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="start the system in debug mode")

    args = parser.parse_args()

    run(args.astrom_file, args.debug)

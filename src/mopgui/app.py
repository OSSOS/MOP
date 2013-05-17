"""
Script for launching the MOP application.
"""

__author__ = "David Rusk <drusk@uvic.ca>"

import argparse

from mopgui.applauncher import AstromFileApplicationLauncher


def run(astrom_file, output_file_name, debug_mode):
    with open(output_file_name, "wb") as output_file:
        AstromFileApplicationLauncher().run(astrom_file, output_file, debug_mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("astrom_file",
                        help="the .astrom file specifying the images to be analyzed")
    parser.add_argument("output_file",
                        help="the output file where accepted sources and their details are written")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="start the system in debug mode")

    args = parser.parse_args()

    run(args.astrom_file, args.output_file, args.debug)

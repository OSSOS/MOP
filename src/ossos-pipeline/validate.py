#!/usr/bin/env python

__author__ = "David Rusk <drusk@uvic.ca>"

import argparse
import tempfile

from ossos.gui import tasks


def launch_app(task, working_directory, output_directory, dry_run):
    # Put import here to avoid the delay loading them.  This allows quick
    # feedback when argparse can tell the arguments are invalid, and makes
    # getting help with the -h flag faster.
    from ossos.gui.app import ValidationApplication

    ValidationApplication(task, working_directory, output_directory, dry_run)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("task", choices=tasks.task_list,
                        help="The task to perform.")
    parser.add_argument("input",
                        help="The directory with files to be processed.")
    parser.add_argument("-o", "--output",
                        required=False,
                        default=tempfile.gettempdir(),
                        help="The directory where any local output files will "
                             "be placed.")
    parser.add_argument("--dry-run",
                        dest="dry_run",
                        action="store_true",
                        help="Do a dry run, where no results are sent to "
                             "VOSpace.")

    args = parser.parse_args()

    if args.dry_run and args.output == tempfile.gettempdir():
        # Don't use tempdir as default for dry runs, use the input directory
        output = args.input
    else:
        output = args.output

    launch_app(args.task, args.input, output, args.dry_run)


if __name__ == "__main__":
    main()

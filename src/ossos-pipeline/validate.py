#!/usr/bin/env python

__author__ = "David Rusk <drusk@uvic.ca>"

import argparse
import tempfile

from ossos.gui import tasks
from ossos.gui.app import ValidationApplication


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

    ValidationApplication(args.task, args.input, args.output, args.dry_run)


if __name__ == "__main__":
    main()

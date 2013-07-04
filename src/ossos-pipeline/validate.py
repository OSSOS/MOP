#!/usr/bin/env python

__author__ = "David Rusk <drusk@uvic.ca>"

import argparse

from ossos.gui import tasks
from ossos.gui.app import ValidationApplication


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("task", choices=tasks.task_list,
                        help="The task to perform.")
    parser.add_argument("directory",
                        help="The directory with files to be processed.")

    args = parser.parse_args()

    ValidationApplication(args.task, args.directory)


if __name__ == "__main__":
    main()

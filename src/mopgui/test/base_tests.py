"""
Base classes for test cases.
"""

import os
import unittest


class FileReadingTestCase(unittest.TestCase):
    def get_abs_path(self, relative_path):
        """
        Get the absolute path of a file in the test directory.

        Args:
          relative_path: str
            path of the file with respect to the base of the test directory
            ex: data/datafile.csv

        Returns:
          absolute_path: str
            The absolute path to the file.
        """
        if relative_path.startswith("/"):
            relative_path = relative_path[1:]
        relative_path.replace("/", os.sep)
        return os.path.join(os.path.dirname(__file__), relative_path)

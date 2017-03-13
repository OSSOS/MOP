__author__ = "David Rusk <drusk@uvic.ca>"

import os
import sys

root_test_dir = os.path.dirname(os.path.dirname(__file__))
sys.path.insert(0, os.path.dirname(root_test_dir))

from tests import testutil


def get_test_data_path(filename):
    return os.path.join(root_test_dir, "../data", filename)


__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains

from mopgui.io import string_utils


class StringUtilsTest(unittest.TestCase):
    def test_split_2_or_more_spaces(self):
        test_string = " 2012 10 21.40516  320.14  2.70  2.90  30000.0   " \
                      "26.92871   29.01125  1584431  "

        assert_that(string_utils.split_2_or_more_spaces(test_string),
                    contains("2012 10 21.40516", "320.14", "2.70", "2.90",
                             "30000.0", "26.92871", "29.01125", "1584431"))


if __name__ == '__main__':
    unittest.main()

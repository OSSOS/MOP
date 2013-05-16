__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, has_length, less_than_or_equal_to, equal_to

from mopgui.io.output import ProvisionalNameGenerator


class ProvisionalNameGeneratorTest(unittest.TestCase):

    def assert_valid_length(self, name):
        assert_that(name, has_length(less_than_or_equal_to(7)))

    def test_generate_name(self):
        undertest = ProvisionalNameGenerator()

        name = undertest.generate_name("1616703")

        self.assert_valid_length(name)

        # TODO
        assert_that(name, equal_to(""))



if __name__ == '__main__':
    unittest.main()

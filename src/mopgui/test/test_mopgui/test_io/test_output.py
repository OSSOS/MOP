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


class Base26ConverterTest(unittest.TestCase):
    def test_boundary(self):
        assert_that(to_base26(0), equal_to("A"))
        assert_that(to_base26(1), equal_to("B"))
        assert_that(to_base26(9999999), equal_to("VWYXJ"))

    def test_realistic_exposure_numbers(self):
        assert_that(to_base26(1616703), equal_to("DNZOX"))
        assert_that(to_base26(1616704), equal_to("DNZOY"))
        assert_that(to_base26(1616705), equal_to("DNZOZ"))

    def test_negative(self):
        self.assertRaises(ValueError, to_base26, -1)


if __name__ == '__main__':
    unittest.main()

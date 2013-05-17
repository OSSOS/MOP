__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, has_length, less_than_or_equal_to, equal_to

from mopgui.io.naming import ProvisionalNameGenerator, to_base26


class ProvisionalNameGeneratorTest(unittest.TestCase):
    def setUp(self):
        self.undertest = ProvisionalNameGenerator()

    def assert_valid_length(self, name):
        assert_that(name, has_length(less_than_or_equal_to(7)))

    def test_exposure_number_not_7_digits(self):
        self.assertRaises(ValueError, self.undertest.generate_name, 123456)
        self.assertRaises(ValueError, self.undertest.generate_name, 12345678)
        self.assertRaises(ValueError, self.undertest.generate_name, 0)
        self.assertRaises(ValueError, self.undertest.generate_name, None)
        self.assertRaises(ValueError, self.undertest.generate_name, "abcdefg")

    def test_generate_names_from_str_no_repeat_exp1(self):
        name = self.undertest.generate_name("1616703")
        self.assert_valid_length(name)
        assert_that(name, equal_to("DNZOX00"))

    def test_generate_names_from_str_no_repeat_exp2(self):
        name = self.undertest.generate_name("1616704")
        self.assert_valid_length(name)
        assert_that(name, equal_to("DNZOY00"))

    def test_generate_names_from_int_no_repeat_exp1(self):
        name = self.undertest.generate_name(1616703)
        self.assert_valid_length(name)
        assert_that(name, equal_to("DNZOX00"))

    def test_generate_names_from_int_no_repeat_exp2(self):
        name = self.undertest.generate_name(1616704)
        self.assert_valid_length(name)
        assert_that(name, equal_to("DNZOY00"))

    def test_generate_names_repeat_exp(self):
        name = self.undertest.generate_name(1616703)
        self.assert_valid_length(name)
        assert_that(name, equal_to("DNZOX00"))

        name2 = self.undertest.generate_name(1616703)
        self.assert_valid_length(name2)
        assert_that(name2, equal_to("DNZOX01"))

        name3 = self.undertest.generate_name(1616703)
        self.assert_valid_length(name3)
        assert_that(name3, equal_to("DNZOX02"))

        name4 = self.undertest.generate_name(1616704)
        self.assert_valid_length(name4)
        assert_that(name4, equal_to("DNZOY00"))


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

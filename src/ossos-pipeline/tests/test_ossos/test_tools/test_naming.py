__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from ossos import naming
from ossos.naming import ProvisionalNameGenerator
from ossos.astrom import Observation, Source, SourceReading


class ProvisionalNameGeneratorTest(unittest.TestCase):
    def setUp(self):
        self.undertest = ProvisionalNameGenerator()

    def create_reading(self, obs):
        """Create a test reading containing the provided observation"""
        return SourceReading(0, 0, 0, 0, 0, 0, 0, 0, obs)

    def create_source(self, expnums):
        """
        Create a test source with default values but the provided exposure numbers.
        """
        readings = [self.create_reading(Observation(expnum, "p", "22")) for expnum in expnums]
        return Source(readings)

    def test_exposure_number_not_7_digits(self):
        self.assertRaises(ValueError, self.undertest.compress_exposure_number, 123456)
        self.assertRaises(ValueError, self.undertest.compress_exposure_number, 12345678)
        self.assertRaises(ValueError, self.undertest.compress_exposure_number, 0)
        self.assertRaises(ValueError, self.undertest.compress_exposure_number, None)
        self.assertRaises(ValueError, self.undertest.compress_exposure_number, "abcdefg")

    def test_compress_exposure_number_from_str_no_repeat_exp1(self):
        compressed = self.undertest.compress_exposure_number("1616703")
        assert_that(compressed, equal_to("DNZOX"))

    def test_compress_exposure_number_from_str_no_repeat_exp2(self):
        compressed = self.undertest.compress_exposure_number("1616704")
        assert_that(compressed, equal_to("DNZOY"))

    def test_compress_exposure_number_from_int_no_repeat_exp1(self):
        compressed = self.undertest.compress_exposure_number(1616703)
        assert_that(compressed, equal_to("DNZOX"))

    def test_compress_exposure_number_from_int_no_repeat_exp2(self):
        compressed = self.undertest.compress_exposure_number(1616704)
        assert_that(compressed, equal_to("DNZOY"))

    def test_generate_names_repeat_exp(self):
        source1 = self.create_source(["1616703"])
        source2 = self.create_source(["1616703"])
        source3 = self.create_source(["1616703"])
        source4 = self.create_source(["1616704"])

        assert_that(self.undertest.name_source(source1), equal_to("DNZOX00"))
        assert_that(self.undertest.name_source(source2), equal_to("DNZOX01"))
        assert_that(self.undertest.name_source(source3), equal_to("DNZOX02"))
        assert_that(self.undertest.name_source(source4), equal_to("DNZOY00"))

    def test_name_source(self):
        source = self.create_source(["1616704", "1616703"])
        assert_that(self.undertest.name_source(source), equal_to("DNZOX00"))

    def test_name_same_source(self):
        source = self.create_source(["1616704", "1616703"])
        assert_that(self.undertest.name_source(source), equal_to("DNZOX00"))
        # Generating the name again should produce the same thing
        assert_that(self.undertest.name_source(source), equal_to("DNZOX00"))

    def test_name_source_different_for_same_obs_diff_src(self):
        source1 = self.create_source(["1616703", "1616704"])
        source2 = self.create_source(["1616703", "1616705"])
        assert_that(self.undertest.name_source(source1), equal_to("DNZOX00"))
        assert_that(self.undertest.name_source(source2), equal_to("DNZOX01"))


class EncodingTest(unittest.TestCase):
    def test_base26_boundary(self):
        assert_that(naming.base26encode(0), equal_to("A"))
        assert_that(naming.base26encode(1), equal_to("B"))
        assert_that(naming.base26encode(9999999), equal_to("VWYXJ"))

    def test_base26_realistic_exposure_numbers(self):
        assert_that(naming.base26encode(1616703), equal_to("DNZOX"))
        assert_that(naming.base26encode(1616704), equal_to("DNZOY"))
        assert_that(naming.base26encode(1616705), equal_to("DNZOZ"))

    def test_base36_encode_1(self):
        assert_that(naming.encode(1, naming.ALPHABET_BASE_36), equal_to("1"))

    def test_base36_encode_10(self):
        assert_that(naming.encode(10, naming.ALPHABET_BASE_36), equal_to("A"))

    def test_base36_encode_100(self):
        assert_that(naming.encode(100, naming.ALPHABET_BASE_36), equal_to("2S"))

    def test_base36_encode_1000(self):
        assert_that(naming.encode(1000, naming.ALPHABET_BASE_36), equal_to("RS"))

    def test_base36_encode_10000(self):
        assert_that(naming.encode(10000, naming.ALPHABET_BASE_36), equal_to("7PS"))


if __name__ == '__main__':
    unittest.main()

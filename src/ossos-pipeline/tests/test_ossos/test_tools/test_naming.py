__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from tests.base_tests import FileReadingTestCase
from ossos import naming
from ossos.astrom import AstromParser
from ossos.naming import ProvisionalNameGenerator


class ProvisionalNameGeneratorTest(FileReadingTestCase):
    def setUp(self):
        self.undertest = ProvisionalNameGenerator()

    def parse_data(self, filename="data/1616688s17.measure3.cands.astrom"):
        return AstromParser().parse(self.get_abs_path(filename))

    def test_generate_name(self):
        astrom_data = self.parse_data()
        source = astrom_data.get_sources()[0]

        assert_that(self.undertest.name_source(source), equal_to("LZuI52T"))

    def test_generated_name_is_padded_if_short(self):
        astrom_data = self.parse_data()
        source = astrom_data.get_sources()[1]

        assert_that(self.undertest.name_source(source), equal_to("LZuIBB0"))


class EncodingTest(unittest.TestCase):
    def test_base26_boundary(self):
        assert_that(naming.base26encode(0), equal_to("A"))
        assert_that(naming.base26encode(1), equal_to("B"))
        assert_that(naming.base26encode(9999999), equal_to("VWYXJ"))

    def test_base26_realistic_exposure_numbers(self):
        assert_that(naming.base26encode(1616703), equal_to("DNZOX"))
        assert_that(naming.base26encode(1616704), equal_to("DNZOY"))
        assert_that(naming.base26encode(1616705), equal_to("DNZOZ"))

    def test_base36_encode_decode_1(self):
        assert_that(naming.base36encode(1), equal_to("1"))
        assert_that(naming.base36decode("1"), equal_to(1))

    def test_base36_encode_10(self):
        assert_that(naming.base36encode(10), equal_to("A"))
        assert_that(naming.base36decode("A"), equal_to(10))

    def test_base36_encode_100(self):
        assert_that(naming.base36encode(100), equal_to("2S"))
        assert_that(naming.base36decode("2S"), equal_to(100))

    def test_base36_encode_10000(self):
        assert_that(naming.base36encode(10000), equal_to("7PS"))
        assert_that(naming.base36decode("7PS"), equal_to(10000))

    def test_base36_encode_pad_short(self):
        assert_that(naming.base36encode(1, pad_length=2), equal_to("01"))

    def test_base36_encode_pad_long(self):
        assert_that(naming.base36encode(10000, pad_length=2), equal_to("7PS"))


if __name__ == '__main__':
    unittest.main()

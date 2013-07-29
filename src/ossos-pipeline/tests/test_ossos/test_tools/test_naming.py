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
        source0 = astrom_data.get_sources()[0]
        reading0 = source0.get_reading(0)

        expected = (naming.base62encode(1616688) +
                    naming.base62encode(int(reading0.x + reading0.y)))
        assert_that(self.undertest.name_source(source0), equal_to(expected))


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

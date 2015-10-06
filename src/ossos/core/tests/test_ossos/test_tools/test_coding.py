__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from ossos import coding


class EncodingDecodingTest(unittest.TestCase):
    def test_base36_encode_decode_1(self):
        assert_that(coding.base36encode(1), equal_to("1"))
        assert_that(coding.base36decode("1"), equal_to(1))

    def test_base36_encode_10(self):
        assert_that(coding.base36encode(10), equal_to("A"))
        assert_that(coding.base36decode("A"), equal_to(10))

    def test_base36_encode_100(self):
        assert_that(coding.base36encode(100), equal_to("2S"))
        assert_that(coding.base36decode("2S"), equal_to(100))

    def test_base36_encode_10000(self):
        assert_that(coding.base36encode(10000), equal_to("7PS"))
        assert_that(coding.base36decode("7PS"), equal_to(10000))

    def test_base36_encode_pad_short(self):
        assert_that(coding.base36encode(1, pad_length=2), equal_to("01"))

    def test_base36_encode_pad_long(self):
        assert_that(coding.base36encode(10000, pad_length=2), equal_to("7PS"))


if __name__ == '__main__':
    unittest.main()

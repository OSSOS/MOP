__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, same_instance

from pymop.patterns import Singleton


class SingletonTest(unittest.TestCase):
    def test_only_one(self):
        undertest = TestSingleton()
        assert_that(undertest.hello, equal_to("World"))
        undertest.hello = "Singleton"
        assert_that(undertest.hello, equal_to("Singleton"))

        undertest2 = TestSingleton()
        assert_that(undertest2.hello, equal_to("Singleton"))
        assert_that(undertest, same_instance(undertest2))


class TestSingleton(object):
    __metaclass__ = Singleton

    def __init__(self):
        self.hello = "World"


if __name__ == '__main__':
    unittest.main()

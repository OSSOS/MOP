__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, contains, same_instance

from test.base_tests import FileReadingTestCase
from mopgui import config


class AppConfigTest(FileReadingTestCase):
    def conffile(self):
        return self.get_abs_path("data/testconf.json")

    def test_read_values(self):
        appconfig = config.AppConfig(configfile=self.conffile())
        assert_that(appconfig.read("testsection1.key1"), equal_to("val1"))
        assert_that(appconfig.read("testsection1.key2"), equal_to(1.2))
        assert_that(appconfig.read("testsection2.s2key1"), equal_to("someval"))

    def test_read_list(self):
        appconfig = config.AppConfig(configfile=self.conffile())
        assert_that(appconfig.read("testsection1.key3"), contains("lval1", "lval2", "lval3"))

    def test_is_singleton(self):
        appconfig = config.AppConfig(configfile=self.conffile())
        config2 = config.AppConfig(configfile=self.conffile())

        assert_that(appconfig, same_instance(config2))

    def test_module_read_function(self):
        configfile = self.conffile()
        assert_that(config.read("testsection1.key2", configfile), equal_to(1.2))


if __name__ == '__main__':
    unittest.main()

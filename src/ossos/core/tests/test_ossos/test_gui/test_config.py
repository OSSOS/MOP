__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, contains, has_length

from tests.base_tests import FileReadingTestCase
from ossos.gui import config
import importlib


class AppConfigTest(FileReadingTestCase):
    def setUp(self):
        importlib.reload(config)

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

    def test_lazy_instantiation(self):
        assert_that(config._configs, has_length(0))
        config.read("testsection1.key1", configfile=self.conffile())
        assert_that(config._configs, has_length(1))
        config.read("testsection1.key2", configfile=self.conffile())
        assert_that(config._configs, has_length(1))

    def test_module_read_function(self):
        configfile = self.conffile()
        assert_that(config.read("testsection1.key2", configfile), equal_to(1.2))


if __name__ == '__main__':
    unittest.main()

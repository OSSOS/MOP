import unittest

from hamcrest import assert_that, equal_to

from test.base_tests import FileReadingTestCase
from mopgui.model.astrodata import AstroDataModel
from mopgui.io.parser import AstromParser


class AstroDataModelTest(FileReadingTestCase):
    def setUp(self):
        testfile = self.get_abs_path("data/1584431p15.measure3.cands.astrom")
        astromdata = AstromParser().parse(testfile)

        self.model = AstroDataModel(astromdata)

    def test_sources_initialized(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        assert_that(self.model.get_source_count(), equal_to(3))

    def test_next_source_previous_source(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))

    def test_next_source_wrap(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        self.model.next_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))

    def test_previous_source_wrap(self):
        assert_that(self.model.get_current_source_number(), equal_to(0))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(2))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(1))
        self.model.previous_source()
        assert_that(self.model.get_current_source_number(), equal_to(0))


if __name__ == '__main__':
    unittest.main()

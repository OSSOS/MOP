import unittest

from hamcrest import assert_that, equal_to, has_length

from test import base_tests
from mopgui.parsing.parser import AstromParser


class ParserTest(base_tests.FileReadingTestCase):
    def setUp(self):
        self.parser = AstromParser()

        # The main test file
        filename = self.get_abs_path("data/1584431p15.measure3.cands.astrom")
        self.astrom_data = self.parser.parse(filename)

    def test_parse_observation_rawnames(self):
        assert_that(self.astrom_data.observations, has_length(3))
        assert_that(self.astrom_data.observations[0].rawname,
                    equal_to("1584431p15"))
        assert_that(self.astrom_data.observations[1].rawname,
                    equal_to("1584449p15"))
        assert_that(self.astrom_data.observations[2].rawname,
                    equal_to("1584453p15"))

    def test_parse_observation_basic_details(self):
        obs0 = self.astrom_data.observations[0]
        obs1 = self.astrom_data.observations[1]
        obs2 = self.astrom_data.observations[2]

        assert_that(obs0.expnum, equal_to("1584431"))
        assert_that(obs0.ftype, equal_to("p"))
        assert_that(obs0.ccdnum, equal_to("15"))

        assert_that(obs1.expnum, equal_to("1584449"))
        assert_that(obs1.ftype, equal_to("p"))
        assert_that(obs1.ccdnum, equal_to("15"))

        assert_that(obs2.expnum, equal_to("1584453"))
        assert_that(obs2.ftype, equal_to("p"))
        assert_that(obs2.ccdnum, equal_to("15"))


if __name__ == '__main__':
    unittest.main()

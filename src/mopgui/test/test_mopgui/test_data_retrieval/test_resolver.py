import unittest

from hamcrest import assert_that, equal_to

from mopgui.data_retrieval.resolver import VOSpaceResolver
from mopgui.parsing.parser import Observation


class ResolverTest(unittest.TestCase):
    def setUp(self):
        self.resolver = VOSpaceResolver()

    def test_resolve_uri(self):
        observation = Observation("1584431", "p", "15")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p.fits"
        assert_that(self.resolver.resolve_uri(observation),
                    equal_to(expected_uri))


if __name__ == '__main__':
    unittest.main()

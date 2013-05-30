__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from pymop.io.parser import Observation
from pymop.io.imgaccess import VOSpaceResolver


class ResolverTest(unittest.TestCase):
    def setUp(self):
        self.resolver = VOSpaceResolver()

    def test_resolve_image_uri(self):
        observation = Observation("1584431", "p", "15")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p.fits"
        assert_that(self.resolver.resolve_image_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_apcor_uri(self):
        observation = Observation("1616681", "p", "22")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd22/1616681p22.apcor"
        assert_that(self.resolver.resolve_apcor_uri(observation),
                    equal_to(expected_uri))

    def test_resolve_apcor_uri_single_digit_ccd(self):
        """Just double checking we don't run into trouble with leading zeros"""
        observation = Observation("1616681", "p", "05")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd05/1616681p05.apcor"
        assert_that(self.resolver.resolve_apcor_uri(observation),
                    equal_to(expected_uri))


if __name__ == '__main__':
    unittest.main()

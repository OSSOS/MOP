__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from ossos.downloads.core import ApcorData


class ApcorDataTest(unittest.TestCase):
    def setUp(self):
        """
        Example data from
        vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd22/1616681p22.apcor
        """
        self.ap_in = 4
        self.ap_out = 15
        self.apcor = 0.19
        self.apcor_err = 0.01

        self.undertest = ApcorData(self.ap_in, self.ap_out, self.apcor,
                                   self.apcor_err)

    def test_basic_properties(self):
        assert_that(self.undertest.ap_in, equal_to(self.ap_in))
        assert_that(self.undertest.ap_out, equal_to(self.ap_out))
        assert_that(self.undertest.apcor, equal_to(self.apcor))
        assert_that(self.undertest.apcor_err, equal_to(self.apcor_err))

    def test_generated_properties(self):
        assert_that(self.undertest.aperture, equal_to(self.ap_in))
        assert_that(self.undertest.sky, equal_to(self.ap_out + 1))
        assert_that(self.undertest.swidth, equal_to(self.ap_in))

    def test_from_raw_string(self):
        rawstr = "4 15   0.19   0.01\n"

        # NOTE: not undertest
        self.parsed = ApcorData.from_string(rawstr)

        assert_that(self.parsed.ap_in, equal_to(self.ap_in))
        assert_that(self.parsed.ap_out, equal_to(self.ap_out))
        assert_that(self.parsed.apcor, equal_to(self.apcor))
        assert_that(self.parsed.apcor_err, equal_to(self.apcor_err))


if __name__ == '__main__':
    unittest.main()

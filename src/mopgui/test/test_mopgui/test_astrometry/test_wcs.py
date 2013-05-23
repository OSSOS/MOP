__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to

from mopgui.astrometry import wcs


class WCSTest(unittest.TestCase):
    """
    Source for test case data:
    http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/821543p.head
    """

    def test_get_order_0(self):
        pv = [[1], [1]]
        assert_that(wcs.get_order(pv), equal_to(0))

    def test_get_order_1(self):
        pv = [range(4), range(4)]
        assert_that(wcs.get_order(pv), equal_to(1))

    def test_get_order_2(self):
        pv = [range(7), range(7)]
        assert_that(wcs.get_order(pv), equal_to(2))

    def test_get_order_3(self):
        pv = [range(11), range(11)]
        assert_that(wcs.get_order(pv), equal_to(3))

    def test_xy2sky_linear(self):
        x = 15000
        y = 20000
        crpix1 = -7535.57493517
        crpix2 = 9808.40914361
        cd = [[5.115244026718E-05, 7.064503033578E-07],
              [-1.280229655229E-07, -5.123112374523E-05]]
        pv = [[-7.030338745606E-03, 1.01755337222, 8.262429361142E-03, 0.00000000000],
              [-6.146513090656E-03, 1.01552885426, 8.259666421752E-03, 0.00000000000]]
        crval1 = 176.486157083
        crval2 = 8.03697351091


if __name__ == '__main__':
    unittest.main()

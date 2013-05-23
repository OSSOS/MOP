__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, close_to

from mopgui.astrometry import wcs

DELTA = 0.000005


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

    def test_xy2sky_nord3(self):
        x = 15000
        y = 20000
        crpix1 = -7535.57493517
        crpix2 = 9808.40914361
        crval1 = 176.486157083
        crval2 = 8.03697351091
        cd = [[5.115244026718E-05, 7.064503033578E-07],
              [-1.280229655229E-07, -5.123112374523E-05]]
        pv = [[-7.030338745606E-03, 1.01755337222, 8.262429361142E-03,
               0.00000000000, -5.910145454849E-04, -7.494178330178E-04,
               -3.470178516657E-04, -2.331150605755E-02, -8.187062772669E-06,
               -2.325429510806E-02, 1.135299506292E-04],
              [-6.146513090656E-03, 1.01552885426, 8.259666421752E-03,
               0.00000000000, -4.567030382243E-04, -6.978676921999E-04,
               -3.732572951216E-04, -2.332572754467E-02, -2.354317291723E-05,
               -2.329623852891E-02, 1.196394469003E-04]]
        nord = 3

        ra, dec = wcs.xy2sky(x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord)

        assert_that(ra, close_to(177.62042274595882, DELTA))
        assert_that(dec, close_to(7.5256071336988679, DELTA))


if __name__ == '__main__':
    unittest.main()
